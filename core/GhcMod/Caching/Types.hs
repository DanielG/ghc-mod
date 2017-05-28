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
module GhcMod.Caching.Types where

import Utils
import Data.Label
import System.Directory.ModTime
import Distribution.Helper

type CacheContents d a = Maybe (ModTime, [FilePath], d, a)
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
  tcCreated :: ModTime,
  -- ^ 'cacheFile' timestamp
  tcFiles     :: [TimedFile]
  -- ^ Timestamped files returned by the cached action
 } deriving (Eq, Ord)

type ChCacheData = (Programs, FilePath, (String, String))
