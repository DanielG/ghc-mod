module Language.Haskell.GhcMod.Caching.Types where

import Utils
import Data.Label
import Data.Version
import Distribution.Helper

type CacheContents d a = Maybe ([FilePath], d, a)
type CacheLens s d a = s :-> CacheContents d a

data Cached m s d a = Cached {
  cacheFile       :: FilePath,
  cacheLens       :: Maybe (CacheLens s d a),
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
  -- * @data@: Arbitrary static input data to cache action. Can be used to
  -- invalidate the cache using something other than file timestamps
  -- i.e. environment tool version numbers
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

type ChCacheData = (Programs, FilePath, FilePath, (Version, [Char]))
