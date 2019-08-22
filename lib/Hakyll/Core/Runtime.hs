--------------------------------------------------------------------------------
module Hakyll.Core.Runtime
    ( run
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent.Async      (Async, async, wait)
import           Control.Concurrent.STM
import           Control.Monad                 (unless)
import           Control.Monad.Except          (ExceptT, runExceptT, throwError)
import           Control.Monad.Reader          (ask)
import           Control.Monad.RWS             (RWST, runRWST)
import qualified Control.Monad.State as State  (get, modify)
import           Control.Monad.Trans           (liftIO)
import           Data.List                     (intercalate)
import           Data.Map                      (Map)
import qualified Data.Map                      as M
import           Data.Set                      (Set)
import qualified Data.Set                      as S
import           System.Exit                   (ExitCode (..))
import           System.FilePath               ((</>))


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Compiler.Require
import           Hakyll.Core.Configuration
import           Hakyll.Core.Dependencies
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Core.Item.SomeItem
import           Hakyll.Core.Logger            (Logger)
import qualified Hakyll.Core.Logger            as Logger
import           Hakyll.Core.Provider
import           Hakyll.Core.Routes
import           Hakyll.Core.Rules.Internal
import           Hakyll.Core.Store             (Store)
import qualified Hakyll.Core.Store             as Store
import           Hakyll.Core.Util.File
import           Hakyll.Core.Writable


--------------------------------------------------------------------------------
run :: Configuration -> Logger -> Rules a -> IO (ExitCode, RuleSet)
run config logger rules = do
    -- Initialization
    Logger.header logger "Initialising..."
    Logger.message logger "Creating store..."
    store <- Store.new (inMemoryCache config) $ storeDirectory config
    Logger.message logger "Creating provider..."
    provider <- newProvider store (shouldIgnoreFile config) $
        providerDirectory config
    Logger.message logger "Running rules..."
    ruleSet  <- runRules rules provider

    -- Get old facts
    mOldFacts <- Store.get store factsKey
    let (oldFacts) = case mOldFacts of Store.Found f -> f
                                       _             -> mempty

    -- Build runtime read/state
    let compilers = rulesCompilers ruleSet
        read'     = RuntimeRead
            { runtimeConfiguration = config
            , runtimeLogger        = logger
            , runtimeProvider      = provider
            , runtimeStore         = store
            , runtimeRoutes        = rulesRoutes ruleSet
            , runtimeUniverse      = M.fromList compilers
            }
    state <- newTVarIO $ RuntimeState'
            { runtimeDone      = S.empty
            , runtimeSnapshots = S.empty
            , runtimeTodo      = M.empty
            , runtimeWorkers   = M.empty
            , runtimeFacts     = oldFacts
            }

    -- Run the program and fetch the resulting state
    result <- runExceptT $ runRWST build read' state
    case result of
        Left e          -> do
            Logger.error logger e
            Logger.flush logger
            return (ExitFailure 1, ruleSet)

        Right (_, sVar, _) -> do
            s <- readTVarIO sVar
            Store.set store factsKey $ runtimeFacts s

            Logger.debug logger "Removing tmp directory..."
            removeDirectory $ tmpDirectory config

            Logger.flush logger
            return (ExitSuccess, ruleSet)
  where
    factsKey = ["Hakyll.Core.Runtime.run", "facts"]


--------------------------------------------------------------------------------
data RuntimeRead = RuntimeRead
    { runtimeConfiguration :: Configuration
    , runtimeLogger        :: Logger
    , runtimeProvider      :: Provider
    , runtimeStore         :: Store
    , runtimeRoutes        :: Routes
    , runtimeUniverse      :: Map Identifier (Compiler SomeItem)
    }


--------------------------------------------------------------------------------
data RuntimeState' = RuntimeState'
    { runtimeDone      :: Set Identifier
    , runtimeSnapshots :: Set (Identifier, Snapshot)
    , runtimeTodo      :: Map Identifier (Compiler SomeItem)
    , runtimeWorkers   :: Map Identifier (Async (Runtime()))
    , runtimeFacts     :: DependencyFacts
    }

type RuntimeState = TVar RuntimeState'
--------------------------------------------------------------------------------
type Runtime a = RWST RuntimeRead () RuntimeState (ExceptT String IO) a


--------------------------------------------------------------------------------
build :: Runtime ()
build = do
    logger <- runtimeLogger <$> ask
    Logger.header logger "Checking for out-of-date items"
    scheduleOutOfDate
    Logger.header logger "Compiling"
    pickAndChase
    Logger.header logger "Success"


--------------------------------------------------------------------------------
scheduleOutOfDate :: Runtime ()
scheduleOutOfDate = do
    logger   <- runtimeLogger   <$> ask
    provider <- runtimeProvider <$> ask
    universe <- runtimeUniverse <$> ask
    facts    <- runtimeFacts    <$> get
    todo     <- runtimeTodo     <$> get

    let identifiers = M.keys universe
        modified    = S.fromList $ flip filter identifiers $
            resourceModified provider

    let (ood, facts', msgs) = outOfDate identifiers modified facts
        todo'               = M.filterWithKey
            (\id' _ -> id' `S.member` ood) universe

    -- Print messages
    mapM_ (Logger.debug logger) msgs

    -- Update facts and todo items
    modify $ \s -> s
        { runtimeDone  = runtimeDone s `S.union`
            (S.fromList identifiers `S.difference` ood)
        , runtimeTodo  = todo `M.union` todo'
        , runtimeFacts = facts'
        }


--------------------------------------------------------------------------------
pickAndChase :: Runtime ()
pickAndChase = do
    todo         <- runtimeTodo    <$> get
    workers      <- runtimeWorkers <$> get
    let todo'     = M.filterWithKey (\k _ -> not $ k `M.member` workers) todo
    case M.minViewWithKey todo' of
        Nothing            -> do
            completedIds <- runtimeDone <$> get
            liftIO . putStrLn $ "1 Completed ids: " ++ (show completedIds)
            liftIO . putStrLn $ "1 Worker ids: " ++ (show $ M.keys workers)
            rs <- liftIO $ traverse wait workers
            _  <- sequence rs -- wait on the workers
            completedIds <- runtimeDone <$> get
            workers      <- runtimeWorkers <$> get
            liftIO . putStrLn $ "2 Completed ids: " ++ (show completedIds)
            liftIO . putStrLn $ "2 Worker ids: " ++ (show $ M.keys workers)
            pure ()
        Just ((id', _), _) -> do
            chase [] id'
            pickAndChase

--------------------------------------------------------------------------------
chase :: [Identifier] -> Identifier -> Runtime ()
chase trail id'
    | id' `elem` trail = throwError $ "Hakyll.Core.Runtime.chase: " ++
        "Dependency cycle detected: " ++ intercalate " depends on "
            (map show $ dropWhile (/= id') (reverse trail) ++ [id'])
    | otherwise        = do
        liftIO . putStrLn $ "chasing " ++ (show id')
        logger   <- runtimeLogger        <$> ask
        todo     <- runtimeTodo          <$> get
        provider <- runtimeProvider      <$> ask
        universe <- runtimeUniverse      <$> ask
        routes   <- runtimeRoutes        <$> ask
        store    <- runtimeStore         <$> ask
        config   <- runtimeConfiguration <$> ask
        Logger.debug logger $ "Processing " ++ show id'

        let compiler = todo M.! id'
            read' = CompilerRead
                { compilerConfig     = config
                , compilerUnderlying = id'
                , compilerProvider   = provider
                , compilerUniverse   = M.keysSet universe
                , compilerRoutes     = routes
                , compilerStore      = store
                , compilerLogger     = logger
                }

        result <- liftIO . async $ runCompiler compiler read'
        let chased = fmap (handleResult trail id') result
        modify $ \s ->
          s { runtimeWorkers = M.insert id' chased (runtimeWorkers s) }
        pure ()

--------------------------------------------------------------------------------
handleResult :: [Identifier] -> Identifier -> (CompilerResult SomeItem) -> Runtime ()
handleResult trail id' result = do
  liftIO . putStrLn $ "Top of Handle Result"
  logger   <- runtimeLogger        <$> ask
  provider <- runtimeProvider      <$> ask
  routes   <- runtimeRoutes        <$> ask
  store    <- runtimeStore         <$> ask
  config   <- runtimeConfiguration <$> ask
  case result of
      -- Rethrow error
      CompilerError [] -> throwError
          "Compiler failed but no info given, try running with -v?"
      CompilerError es -> throwError $ intercalate "; " es

      -- Signal that a snapshot was saved ->
      CompilerSnapshot snapshot c -> do
          -- Update info. The next 'chase' will pick us again at some
          -- point so we can continue then.
          modify $ \s -> s
              { runtimeSnapshots =
                  S.insert (id', snapshot) (runtimeSnapshots s)
              , runtimeTodo      = M.insert id' c (runtimeTodo s)
              }

      -- Huge success
      CompilerDone (SomeItem item) cwrite -> do
          -- Print some info
          let facts = compilerDependencies cwrite
              cacheHits
                  | compilerCacheHits cwrite <= 0 = "updated"
                  | otherwise                     = "cached "
          Logger.message logger $ cacheHits ++ " " ++ show id'

          -- Sanity check
          unless (itemIdentifier item == id') $ throwError $
              "The compiler yielded an Item with Identifier " ++
              show (itemIdentifier item) ++ ", but we were expecting " ++
              "an Item with Identifier " ++ show id' ++ " " ++
              "(you probably want to call makeItem to solve this problem)"

          -- Write if necessary
          (mroute, _) <- liftIO $ runRoutes routes provider id'
          case mroute of
              Nothing    -> return ()
              Just route -> do
                  let path = destinationDirectory config </> route
                  liftIO $ makeDirectories path
                  liftIO $ write path item
                  Logger.debug logger $ "Routed to " ++ path

          -- Save! (For load)
          liftIO $ save store item

          liftIO . putStrLn $ "In compiler done"
          -- Update state
          modify $ \s -> s
              { runtimeDone  = S.insert id' (runtimeDone s)
              , runtimeTodo  = M.delete id' (runtimeTodo s)
              , runtimeFacts = M.insert id' facts (runtimeFacts s)
              }

      -- Try something else first
      CompilerRequire dep c -> do
          liftIO . putStrLn $ "In compilerRequire"
          -- Update the compiler so we don't execute it twice
          let (depId, depSnapshot) = dep
          var <- State.get
          (action, depDone) <- liftIO . atomically $ do
            done      <- runtimeDone      <$> readTVar var
            snapshots <- runtimeSnapshots <$> readTVar var

            -- Done if we either completed the entire item (runtimeDone) or
            -- if we previously saved the snapshot (runtimeSnapshots).
            let depDone =
                    depId `S.member` done ||
                    (depId, depSnapshot) `S.member` snapshots

            modifyTVar' var $ \s -> s {
                  runtimeTodo = M.insert id'
                    (if depDone then c else compilerResult result)
                    (runtimeTodo s)
                }

            let runtimeAction = if depDone
                                then chase trail id'
                                else chase (id' : trail) depId
            pure (runtimeAction, depDone)

          action
          -- If the required item is already compiled, continue, or, start
          -- chasing that
          Logger.debug logger $ "Require " ++ show depId ++
              " (snapshot " ++ depSnapshot ++ "): " ++
              (if depDone then "OK" else "chasing")
          eThread <- liftIO . atomically $ do
            workers <- runtimeWorkers <$> readTVar var
            pure $ M.lookup (if depDone then id' else depId) workers
          case eThread of
            Nothing -> throwError concurrencyException
            Just t  -> liftIO (wait t) >> pure ()

--------------------------------------------------------------------------------
-- WARNING: It is very easy to accidentally use an out-of-date
-- version of RuntimeState' when updating it. Be very careful
-- that your modify function only uses the provided reference
modify :: (RuntimeState' -> RuntimeState') -> Runtime ()
modify f = do
  var <- State.get
  liftIO . atomically $ do
    modifyTVar' var f

--------------------------------------------------------------------------------
get :: Runtime RuntimeState'
get = State.get >>= \var -> liftIO (readTVarIO var)

--------------------------------------------------------------------------------
concurrencyException :: String
concurrencyException =
  "Concurrency Exception. This should never happen. Please report this " ++
  "to the Hakyll issue tracker. In the mean time, try running Hakyll " ++
  "in single threaded mode using '+RTS -N1'."
