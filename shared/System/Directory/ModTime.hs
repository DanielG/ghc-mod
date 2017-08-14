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
{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}
module System.Directory.ModTime where

import Control.Applicative
import Control.DeepSeq
import Data.Binary
#if MIN_VERSION_directory(1,2,0)
import Data.Time (UTCTime(..), Day(..), getCurrentTime)
#else
import System.Time (ClockTime(..), getClockTime)
#endif
import System.Directory
import Prelude

#if MIN_VERSION_directory(1,2,0)

newtype ModTime = ModTime UTCTime
    deriving (Eq, Ord, NFData)
getCurrentModTime = ModTime <$> getCurrentTime

instance Binary ModTime where
    put (ModTime (UTCTime (ModifiedJulianDay day) difftime)) =
        put day >> put (toRational difftime)
    get =
        ModTime <$> (UTCTime <$> (ModifiedJulianDay <$> get) <*> (fromRational <$> get))

#else

newtype ModTime = ModTime ClockTime
    deriving (Eq, Ord)
getCurrentModTime = ModTime <$> getClockTime

instance Binary ModTime where
    put (ModTime (TOD s ps)) =
        put s >> put ps
    get =
        ModTime <$> (TOD <$> get <*> get)

instance NFData ModTime where
    rnf (ModTime (TOD s ps)) =
        s `seq` ps `seq` (ModTime $! TOD s ps) `seq` ()

#endif

getCurrentModTime :: IO ModTime

getModTime :: FilePath -> IO ModTime
getModTime f = ModTime <$> getModificationTime f
