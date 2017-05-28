-- ghc-mod: Happy Haskell Hacking
-- Copyright (C) 2015  Daniel Gröber <dxld ÄT darkboxed DOT org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE CPP, OverloadedStrings #-}
module GhcMod.Caching (
    module GhcMod.Caching
  , module GhcMod.Caching.Types
  ) where

import Control.Arrow (first)
import Control.Monad
import Control.Monad.Trans.Maybe
#if !MIN_VERSION_binary(0,7,0)
import Control.Exception
#endif
import Data.Maybe
import Data.Binary hiding (get)
import Data.Version
import Data.Label
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import System.FilePath
import System.Directory.ModTime
import Utils (TimedFile(..), timeMaybe, mightExist)
import Paths_ghc_mod (version)
import Prelude

import GhcMod.Monad.Types
import GhcMod.Caching.Types
import GhcMod.Logging

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
          t <- liftIO $ getCurrentModTime
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

   lbsToStrict = BS.concat . LBS.toChunks
   lbsFromStrict bs = LBS.fromChunks [bs]

   writeCache tcfs ma cause = do
     (ifs', a) <- (cachedAction cd) tcfs d ma
     t <- liftIO $ getCurrentModTime
     gmLog GmDebug "" $ (text "regenerating cache") <+>: text (cacheFile cd)
                                                    <+> parens (text cause)
     case cacheLens cd of
       Nothing -> return ()
       Just label -> do
         gmLog GmDebug "" $ (text "writing memory cache") <+>: text (cacheFile cd)
         setLabel label $ Just (t, ifs', d, a)

     let c = BS.append cacheHeader $ lbsToStrict $ encode (t, ifs', d, a)

     liftIO $ BS.writeFile (dir </> cacheFile cd) c

     return a

   setLabel l x = do
     s <- gmsGet
     gmsPut $ set l x s

   readCache :: m (Maybe (ModTime, [FilePath], d, a))
   readCache = runMaybeT $ do
       case cacheLens cd of
         Just label -> do
             c <- MaybeT (get label `liftM` gmsGet) `mplus` readCacheFromFile
             setLabel label $ Just c
             return c
         Nothing ->
             readCacheFromFile

   readCacheFromFile :: MaybeT m (ModTime, [FilePath], d, a)
   readCacheFromFile = do
         f <- MaybeT $ liftIO $ mightExist $ cacheFile cd
         readCacheFromFile' f

   readCacheFromFile' :: FilePath -> MaybeT m (ModTime, [FilePath], d, a)
   readCacheFromFile' f = MaybeT $ do
     gmLog GmDebug "" $ (text "reading cache") <+>: text (cacheFile cd)
     cc <- liftIO $ BS.readFile f
     case first BS8.words $ BS8.span (/='\n') cc of
       (["Written", "by", "ghc-mod", ver], rest)
           | BS8.unpack ver == showVersion version ->
            either (const Nothing) Just
                `liftM` decodeE (lbsFromStrict $ BS.drop 1 rest)
       _ -> return Nothing

   decodeE b = do
#if MIN_VERSION_binary(0,7,0)
     return $ case decodeOrFail b of
       Left (_rest, _offset, errmsg) -> Left errmsg
       Right (_reset, _offset, a) -> Right a
#else
     ea <- liftIO $ try $ evaluate $ decode b
     return $ case ea of
       Left (ErrorCall errmsg) -> Left errmsg
       Right a -> Right a

#endif

timeCacheInput :: MonadIO m => FilePath -> [FilePath] -> m [TimedFile]
timeCacheInput dir ifs = liftIO $ do
    ins <- (timeMaybe . (dir </>)) `mapM` ifs
    return $ catMaybes ins

invalidatingInputFiles :: TimedCacheFiles -> [FilePath]
invalidatingInputFiles (TimedCacheFiles tcreated tcfs) =
    map tfPath $
        -- get input files older than tcfile
        filter ((TimedFile "" tcreated)<) tcfs
