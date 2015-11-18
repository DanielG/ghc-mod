{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.GhcMod.Caching (
    module Language.Haskell.GhcMod.Caching
  , module Language.Haskell.GhcMod.Caching.Types
  ) where

import Control.Arrow (first)
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Binary (Binary, encode, decodeOrFail)
import Data.Version
import Data.Label
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Time (UTCTime, getCurrentTime)
import System.FilePath
import Utils (TimedFile(..), timeMaybe, mightExist)
import Paths_ghc_mod (version)

import Language.Haskell.GhcMod.Monad.Types
import Language.Haskell.GhcMod.Caching.Types
import Language.Haskell.GhcMod.Logging

-- | Cache a MonadIO action with proper invalidation.
cached :: forall m a d. (Gm m, MonadIO m, Binary a, Eq d, Binary d, Show d)
       => FilePath -- ^ Directory to prepend to 'cacheFile'
       -> Cached m GhcModState d a -- ^ Cache descriptor
       -> d
       -> m a
cached dir cd d = do
    mcc <- readCache
    case mcc of
      Nothing -> do
          t <- liftIO $ getCurrentTime
          writeCache (TimedCacheFiles t []) Nothing "cache missing or unreadable"
      Just (t, ifs, d', a) | d /= d' -> do
          tcfs <- timeCacheInput dir ifs
          writeCache (TimedCacheFiles t tcfs) (Just a) $ "input data changed" -- ++ "   was: " ++ show d ++ "  is: " ++ show d'
      Just (t, ifs, _, a) -> do
          tcfs <- timeCacheInput dir ifs
          case invalidatingInputFiles $ TimedCacheFiles t tcfs of
            [] -> return a
            _  -> writeCache (TimedCacheFiles t tcfs) (Just a) "input files changed"

 where
   cacheHeader = BS8.pack $ "Written by ghc-mod " ++ showVersion version ++ "\n"

   writeCache tcfs ma cause = do
     (ifs', a) <- (cachedAction cd) tcfs d ma
     t <- liftIO $ getCurrentTime
     gmLog GmDebug "" $ (text "regenerating cache") <+>: text (cacheFile cd)
                                                    <+> parens (text cause)
     case cacheLens cd of
       Nothing -> return ()
       Just label -> do
         gmLog GmDebug "" $ (text "writing memory cache") <+>: text (cacheFile cd)
         setLabel label $ Just (t, ifs', d, a)

     let c = BS.append cacheHeader $ LBS.toStrict $ encode (t, ifs', d, a)

     liftIO $ BS.writeFile (dir </> cacheFile cd) c

     return a

   setLabel l x = do
     s <- gmsGet
     gmsPut $ set l x s

   readCache :: m (Maybe (UTCTime, [FilePath], d, a))
   readCache = runMaybeT $ do
       case cacheLens cd of
         Just label -> do
             c <- MaybeT (get label `liftM` gmsGet) `mplus` readCacheFromFile
             setLabel label $ Just c
             return c
         Nothing ->
             readCacheFromFile

   readCacheFromFile :: MaybeT m (UTCTime, [FilePath], d, a)
   readCacheFromFile = do
         f <- MaybeT $ liftIO $ mightExist $ cacheFile cd
         readCacheFromFile' f

   readCacheFromFile' :: FilePath -> MaybeT m (UTCTime, [FilePath], d, a)
   readCacheFromFile' f = MaybeT $ do
     gmLog GmDebug "" $ (text "reading cache") <+>: text (cacheFile cd)
     cc <- liftIO $ BS.readFile f
     case first BS8.words $ BS8.span (/='\n') cc of
       (["Written", "by", "ghc-mod", ver], rest)
           | BS8.unpack ver == showVersion version ->
            return $ either (const Nothing) Just $
                decodeE $ LBS.fromStrict $ BS.drop 1 rest
       _ -> return Nothing

   decodeE b = do
     case decodeOrFail b of
       Left (_rest, _offset, errmsg) -> Left errmsg
       Right (_reset, _offset, a) -> Right a

timeCacheInput :: MonadIO m => FilePath -> [FilePath] -> m [TimedFile]
timeCacheInput dir ifs = liftIO $ do
    ins <- (timeMaybe . (dir </>)) `mapM` ifs
    return $ catMaybes ins

invalidatingInputFiles :: TimedCacheFiles -> [FilePath]
invalidatingInputFiles (TimedCacheFiles tcreated tcfs) =
    map tfPath $
        -- get input files older than tcfile
        filter ((TimedFile "" tcreated)<) tcfs
