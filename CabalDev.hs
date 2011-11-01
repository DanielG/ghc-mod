module CabalDev (modifyOptions) where

{-
If the directory 'cabal-dev/packages-X.X.X.conf' exists, add it to the 
options ghc-mod uses to check the source.  Otherwise just pass it on.
-}

import Data.Maybe             (listToMaybe)
import System.FilePath.Find
import System.FilePath.Posix  (splitPath,joinPath)
import System.Posix.Directory (getWorkingDirectory)
import System.Directory

import Types

modifyOptions :: Options -> IO Options
modifyOptions opts =
  fmap (has_cdev opts) findCabalDev
 where
   has_cdev :: Options -> Maybe String -> Options
   has_cdev op Nothing = op
   has_cdev op (Just path) = addPath op path

findCabalDev :: IO (Maybe String)
findCabalDev =
  getWorkingDirectory >>= searchIt . splitPath

addPath :: Options -> String -> Options
addPath orig_opts path = do
  let orig_paths = packageConfs orig_opts
  orig_opts { 
    packageConfs = orig_paths ++ [path] }

searchIt :: [FilePath] -> IO (Maybe FilePath)
searchIt [] = return Nothing
searchIt path = do
  a <- doesDirectoryExist (mpath path)
  case a of
    False -> return Nothing
    True -> do
      b <- find always (fileName ~~? "packages*.conf") $ mpath path
      maybe (searchIt $ init path) (return . Just) $ listToMaybe b
 where
   mpath a = (joinPath a) ++ "cabal-dev/"

