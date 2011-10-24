
module CabalDev where

import Maybe                  (listToMaybe)
import System.FilePath.Find
import System.FilePath.Posix
import System.Posix.Directory

import Types

modify_options :: Options -> IO Options
modify_options opts =
  fmap (maybe opts (add_path opts)) find_cabal_dev

add_path :: Options -> String -> Options
add_path orig_opts path = do
  let orig_paths = packageConfs orig_opts
  orig_opts {
    packageConfs = orig_paths ++ [path]
  }

find_cabal_dev :: IO (Maybe String)
find_cabal_dev =
  getWorkingDirectory >>= (searchit . splitPath)

searchit :: [FilePath] -> IO (Maybe FilePath)
searchit [] = return Nothing
searchit p = do
  lx <- find always (fileName ~~? "packages*.conf") $ joinPath p
  maybe (searchit $ init p) (return . Just) $ listToMaybe lx
