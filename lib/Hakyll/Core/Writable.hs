--------------------------------------------------------------------------------
-- | Describes writable items; items that can be saved to the disk
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Hakyll.Core.Writable
    ( Writable (..)
    ) where


--------------------------------------------------------------------------------
import qualified Data.ByteString                 as SB
import qualified Data.ByteString.Lazy            as LB
import           Data.Word                       (Word8)
import           Text.Blaze.Html                 (Html)
import           Text.Blaze.Html.Renderer.String (renderHtml)


--------------------------------------------------------------------------------
import           Hakyll.Core.Item
import           Hakyll.Core.Util.File           (attemptFileOperation)


--------------------------------------------------------------------------------
-- | Describes an item that can be saved to the disk
class Writable a where
    -- | Save an item to the given filepath
    write :: FilePath -> Item a -> IO ()


--------------------------------------------------------------------------------
instance Writable () where
    write _ _ = return ()


--------------------------------------------------------------------------------
instance Writable [Char] where
    write p i = attemptFileOperation p (\path -> writeFile path $ itemBody i)


--------------------------------------------------------------------------------
instance Writable SB.ByteString where
    write p i = attemptFileOperation p (\path -> SB.writeFile path $ itemBody i)


--------------------------------------------------------------------------------
instance Writable LB.ByteString where
    write p i = attemptFileOperation p (\path -> LB.writeFile path $ itemBody i)


--------------------------------------------------------------------------------
instance Writable [Word8] where
    write p i = attemptFileOperation p (\path -> write path $ fmap SB.pack i)


--------------------------------------------------------------------------------
instance Writable Html where
    write p i = attemptFileOperation p (\path -> write path $ fmap renderHtml i)
