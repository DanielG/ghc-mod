module Language.Haskell.GhcMod.Caching where

import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Serialize
import qualified Data.ByteString as BS
import System.FilePath
import Utils (TimedFile(..), timeMaybe, mightExist)

import Language.Haskell.GhcMod.Monad.Types
import Language.Haskell.GhcMod.Logging

import Utils

data Cached m d a =
     Cached { cacheFile       :: FilePath,
              cachedAction    :: TimedCacheFiles -> d -> Maybe a -> m ([FilePath], a)
              -- ^ The cached action, will only run if
              -- * The cache doesn\'t exist yet
              -- * The cache exists and 'inputData' changed
              -- * any files in 'inputFiles' are older than 'cacheFile'.
            }

data TimedCacheFiles =
     TimedCacheFiles { tcCacheFile :: Maybe TimedFile,
                       tcFiles     :: [TimedFile]
                     }

-- | Cache a MonadIO action with proper invalidation.
cached :: forall m a d. (MonadIO m, GmLog m, Serialize a, Eq d, Serialize d)
       => FilePath -- ^ Directory to prepend to 'cacheFile'
       -> Cached m d a -- ^ Cache descriptor
       -> d
       -> m a
cached dir cd d = do
    mcc <- readCache
    tcfile <- liftIO $ timeMaybe (cacheFile cd)
    let defTcf = TimedCacheFiles tcfile []

    case mcc of
      Nothing -> writeCache defTcf Nothing "cache missing"
      Just (ifs, d', a) | d /= d' -> do
          tcf <- timeCacheInput dir (cacheFile cd) ifs
          writeCache tcf (Just a) "input data changed"
      Just (ifs, _, a) -> do
                    tcf <- timeCacheInput dir (cacheFile cd) ifs
                    let invifs = invalidatingInputFiles tcf
                    case invifs of
                      Nothing -> writeCache tcf (Just a) "cache missing, existed a sec ago WTF?"
                      Just [] -> return a
                      Just _ -> writeCache tcf (Just a) "input files changed"

 where
   writeCache tcf ma cause = do
     (ifs', a) <- (cachedAction cd) tcf d ma
     gmLog GmDebug "" $ (text "regenerating cache") <+>: text (cacheFile cd)
                                                    <+> parens (text cause)
     liftIO $ BS.writeFile (dir </> cacheFile cd) $ encode (ifs', d, a)
     return a

   readCache :: m (Maybe ([FilePath], d, a))
   readCache = runMaybeT $ do
       f <- MaybeT $ liftIO $ mightExist $ cacheFile cd
       MaybeT $ readCache' f
    where
       readCache' f = do
         gmLog GmDebug "" $ (text "reading cache") <+>: text (cacheFile cd)
         cc <- liftIO $ BS.readFile f
         return $ either (const Nothing) Just $ decode cc

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
