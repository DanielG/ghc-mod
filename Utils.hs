{-# LANGUAGE CPP #-}
module Utils where

import Control.Applicative
import Data.Traversable
import System.Directory

#if MIN_VERSION_directory(1,2,0)
import Data.Time (UTCTime)
#else
import System.Time (ClockTime)
#endif
import Prelude


#if MIN_VERSION_directory(1,2,0)
type ModTime = UTCTime
#else
type ModTime = ClockTime
#endif

data TimedFile = TimedFile { tfPath :: FilePath, tfTime :: ModTime }
                 deriving (Eq, Show)

instance Ord TimedFile where
    compare (TimedFile _ a) (TimedFile _ b) = compare a b

timeFile :: FilePath -> IO TimedFile
timeFile f = TimedFile <$> pure f <*> getModificationTime f

mightExist :: FilePath -> IO (Maybe FilePath)
mightExist f = do
  exists <- doesFileExist f
  return $ if exists then (Just f) else (Nothing)

timeMaybe :: FilePath -> IO (Maybe TimedFile)
timeMaybe f = traverse timeFile =<< mightExist f
