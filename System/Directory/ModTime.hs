{-# LANGUAGE CPP #-}
module System.Directory.ModTime where

import Control.Applicative
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
    deriving (Eq, Ord)
getCurrentModTime = ModTime <$> getCurrentTime

instance Binary ModTime where
    put (ModTime (UTCTime (ModifiedJulianDay day) difftime)) =
        put day >> put (toRational difftime)
    get =
        ModTime <$> (UTCTime <$> (ModifiedJulianDay <$> get) <*> (fromRational <$> get))

#else

newtype ModTime = ModTime ClockTime
    deriving (Eq, Ord, Show)
getCurrentModTime = ModTime <$> getClockTime

instance Binary ModTime where
    put (ModTime (TOD s ps)) =
        put s >> put ps
    get =
        ModTime . TOD <$> get <*> get

#endif

getCurrentModTime :: IO ModTime

getModTime :: FilePath -> IO ModTime
getModTime f = ModTime <$> getModificationTime f
