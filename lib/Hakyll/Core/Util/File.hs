--------------------------------------------------------------------------------
-- | A module containing various file utility functions
module Hakyll.Core.Util.File
    ( makeDirectories
    , getRecursiveContents
    , removeDirectory
    , attemptOpenFile
    , attemptReadFile
    , attemptFileOperation
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent  (threadDelay)
import           Control.Exception   (try)
import           Control.Monad       (filterM, forM, when)
import           System.Directory    (createDirectoryIfMissing,
                                      doesDirectoryExist, getDirectoryContents,
                                      removeDirectoryRecursive)
import           System.FilePath     (takeDirectory, (</>))
import           System.IO           (Handle, IOMode, openFile, readFile)
import           System.IO.Error     (ioError, isAlreadyInUseError)


--------------------------------------------------------------------------------
-- | Given a path to a file, try to make the path writable by making
--   all directories on the path.
makeDirectories :: FilePath -> IO ()
makeDirectories = createDirectoryIfMissing True . takeDirectory


--------------------------------------------------------------------------------
-- | Get all contents of a directory.
getRecursiveContents :: (FilePath -> IO Bool)  -- ^ Ignore this file/directory
                     -> FilePath               -- ^ Directory to search
                     -> IO [FilePath]          -- ^ List of files found
getRecursiveContents ignore top = go ""
  where
    isProper x
        | x `elem` [".", ".."] = return False
        | otherwise            = not <$> ignore x

    go dir     = do
        dirExists <- doesDirectoryExist (top </> dir)
        if not dirExists
            then return []
            else do
                names <- filterM isProper =<< getDirectoryContents (top </> dir)
                paths <- forM names $ \name -> do
                    let rel = dir </> name
                    isDirectory <- doesDirectoryExist (top </> rel)
                    if isDirectory
                        then go rel
                        else return [rel]

                return $ concat paths


--------------------------------------------------------------------------------
removeDirectory :: FilePath -> IO ()
removeDirectory fp = do
    e <- doesDirectoryExist fp
    when e $ removeDirectoryRecursive fp

--------------------------------------------------------------------------------
attemptOpenFile :: FilePath -> IOMode -> IO Handle
attemptOpenFile path mode = attemptFileOperation path (\p -> openFile p mode)

--------------------------------------------------------------------------------
attemptReadFile :: FilePath -> IO String
attemptReadFile path = attemptFileOperation path readFile

--------------------------------------------------------------------------------
attemptFileOperation :: FilePath -> (FilePath -> IO a) -> IO a
attemptFileOperation path op = go (10 :: Int) where
  go attemptNo = do
    eitherA <- try $ op path
    case eitherA of
      Left err -> do
        if isAlreadyInUseError err && attemptNo > 0
        then do
          putStrLn $ "Remove this message: in attemptFileOperation. Something failed, looping: " ++ show attemptNo
          threadDelay 1000000
          go (attemptNo - 1)
        else ioError err
      Right a -> pure a
