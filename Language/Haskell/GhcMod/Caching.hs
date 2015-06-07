module Language.Haskell.GhcMod.Caching where

import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Serialize
import qualified Data.ByteString as BS
import System.FilePath
import Utils (TimedFile(..), timeMaybe, mightExist)

import Language.Haskell.GhcMod.Monad.Types
import Language.Haskell.GhcMod.Logging

data Cached m d a = Cached {
  cacheFile       :: FilePath,


  cachedAction    :: TimedCacheFiles
                  -> d
                  -> Maybe a
                  -> m ([FilePath], a)

  -- ^ @cachedAction tcf data ma@
  --
  -- * @tcf@: Input file timestamps. Not technically necessary, just an
  -- optimizazion when knowing which input files changed can make updating the
  -- cache faster
  --
  -- * @data@: Arbitrary static input data can be used to invalidate the cache
  -- using something other than file timestamps i.e. environment tool version
  -- numbers
  --
  -- * @ma@: Cached data if it existed
  --
  -- Returns:
  --
  -- * @fst@: Input files used in generating the cache
  --
  -- * @snd@: Cache data, will be stored alongside the static input data in the
  --   'cacheFile'
  --
  -- The cached action, will only run if one of the following is true:
  --
  -- * 'cacheFile' doesn\'t exist yet
  -- * 'cacheFile' exists and 'inputData' changed
  -- * any files returned by the cached action changed
 }

data TimedCacheFiles = TimedCacheFiles {
  tcCacheFile :: Maybe TimedFile,
  -- ^ 'cacheFile' timestamp
  tcFiles     :: [TimedFile]
  -- ^ Timestamped files returned by the cached action
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
    case mcc of
      Nothing ->
          writeCache (TimedCacheFiles tcfile []) Nothing "cache missing"
      Just (ifs, d', a) | d /= d' -> do
          tcf <- timeCacheInput dir (cacheFile cd) ifs
          writeCache tcf (Just a) "input data changed"
      Just (ifs, _, a) -> do
          tcf <- timeCacheInput dir (cacheFile cd) ifs
          case invalidatingInputFiles tcf of
            Just [] -> return a
            Just _  -> writeCache tcf (Just a) "input files changed"
            Nothing -> writeCache tcf (Just a) "cache missing, existed a sec ago WTF?"

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
