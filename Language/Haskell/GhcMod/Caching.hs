{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.GhcMod.Caching (
    module Language.Haskell.GhcMod.Caching
  , module Language.Haskell.GhcMod.Caching.Types
  ) where

import Control.Arrow (first)
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Serialize (Serialize, encode, decode)
import Data.Version
import Data.Label
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import System.FilePath
import Utils (TimedFile(..), timeMaybe, mightExist)
import Paths_ghc_mod (version)

import Language.Haskell.GhcMod.Monad.Types
import Language.Haskell.GhcMod.Caching.Types
import Language.Haskell.GhcMod.Logging

-- | Cache a MonadIO action with proper invalidation.
cached :: forall m a d. (Gm m, MonadIO m, Serialize a, Eq d, Serialize d, Show d)
       => FilePath -- ^ Directory to prepend to 'cacheFile'
       -> Cached m GhcModState d a -- ^ Cache descriptor
       -> d
       -> m a
cached dir cd d = do
    mcc <- readCache
    tcfile <- liftIO $ timeMaybe (cacheFile cd)
    case mcc of
      Nothing ->
          writeCache (TimedCacheFiles tcfile []) Nothing "cache missing or unreadable"
      Just (ifs, d', a) | d /= d' -> do
          tcf <- timeCacheInput dir (cacheFile cd) ifs
          writeCache tcf (Just a) $ "input data changed" -- ++ "   was: " ++ show d ++ "  is: " ++ show d'
      Just (ifs, _, a) -> do
          tcf <- timeCacheInput dir (cacheFile cd) ifs
          case invalidatingInputFiles tcf of
            Just [] -> return a
            Just _  -> writeCache tcf (Just a) "input files changed"
            Nothing -> writeCache tcf (Just a) "cache missing, existed a sec ago WTF?"

 where
   cacheHeader = BS8.pack $ "Written by ghc-mod " ++ showVersion version ++ "\n"

   writeCache tcf ma cause = do
     (ifs', a) <- (cachedAction cd) tcf d ma
     gmLog GmDebug "" $ (text "regenerating cache") <+>: text (cacheFile cd)
                                                    <+> parens (text cause)
     case cacheLens cd of
       Nothing -> return ()
       Just label -> do
         gmLog GmDebug "" $ (text "writing memory cache") <+>: text (cacheFile cd)
         setLabel label $ Just (ifs', d, a)

     liftIO $ BS.writeFile (dir </> cacheFile cd) $
         BS.append cacheHeader $ encode (ifs', d, a)
     return a

   setLabel l x = do
     s <- gmsGet
     gmsPut $ set l x s

   readCache :: m (Maybe ([FilePath], d, a))
   readCache = runMaybeT $ do
       case cacheLens cd of
         Just label -> do
             c <- MaybeT (get label `liftM` gmsGet) `mplus` readCacheFromFile
             setLabel label $ Just c
             return c
         Nothing ->
             readCacheFromFile

   readCacheFromFile = do
         f <- MaybeT $ liftIO $ mightExist $ cacheFile cd
         readCacheFromFile' f

   readCacheFromFile' f = MaybeT $ do
     gmLog GmDebug "" $ (text "reading cache") <+>: text (cacheFile cd)
     cc <- liftIO $ BS.readFile f
     case first BS8.words $ BS8.span (/='\n') cc of
       (["Written", "by", "ghc-mod", ver], rest)
           | BS8.unpack ver == showVersion version ->
            return $ either (const Nothing) Just $ decode $ BS.drop 1 rest
       _ -> return Nothing

timeCacheInput :: MonadIO m => FilePath -> FilePath -> [FilePath] -> m TimedCacheFiles
timeCacheInput dir cfile ifs = liftIO $ do
    -- TODO: is checking the times this way around race free?
    ins <- (timeMaybe . (dir </>)) `mapM` ifs
    mtcfile <- timeMaybe cfile
    return $ TimedCacheFiles mtcfile (catMaybes ins)

invalidatingInputFiles :: TimedCacheFiles -> Maybe [FilePath]
invalidatingInputFiles tcf =
    case tcCacheFile tcf of
      Nothing -> Nothing
      Just tcfile -> Just $ map tfPath $
                     -- get input files older than tcfile
                     filter (tcfile<) $ tcFiles tcf
