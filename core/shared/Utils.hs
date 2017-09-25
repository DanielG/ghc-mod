{-# LANGUAGE CPP #-}
module Utils where

import Control.Applicative
import Data.Traversable
import System.Directory
import System.Directory.ModTime

import Prelude

data TimedFile = TimedFile { tfPath :: FilePath, tfTime :: ModTime }
                 deriving (Eq)

instance Ord TimedFile where
    compare (TimedFile _ a) (TimedFile _ b) = compare a b

timeFile :: FilePath -> IO TimedFile
timeFile f = TimedFile <$> pure f <*> getModTime f

mightExist :: FilePath -> IO (Maybe FilePath)
mightExist f = do
  exists <- doesFileExist f
  return $ if exists then (Just f) else (Nothing)

timeMaybe :: FilePath -> IO (Maybe TimedFile)
timeMaybe f = traverse timeFile =<< mightExist f
