module CabalDev (modify_options) where

{-
If the directory 'cabal-dev/packages-X.X.X.conf' exists, add it to the 
options ghc-mod uses to check the source.  Otherwise just pass it on.
-}

import Maybe                  (listToMaybe)
import System.FilePath.Find
import System.FilePath.Posix (splitPath,joinPath)
import System.Posix.Directory (getWorkingDirectory)

import Types

modify_options :: Options -> IO Options
modify_options opts =
  fmap (has_cdev opts) find_cabal_dev
 where
   has_cdev :: Options -> Maybe String -> Options
   has_cdev op Nothing = op
   has_cdev op (Just path) = add_path op path

find_cabal_dev :: IO (Maybe String)
find_cabal_dev =
  getWorkingDirectory >>= (searchit . splitPath)

add_path :: Options -> String -> Options
add_path orig_opts path = do
  let orig_paths = packageConfs orig_opts
  orig_opts { packageConfs = orig_paths ++ [path] }

searchit :: [FilePath] -> IO (Maybe FilePath)
searchit [] = return Nothing
searchit p = do
  lx <- find always (fileName ~~? "packages*.conf") $ joinPath p
  maybe (searchit $ init p) (return . Just) $ listToMaybe lx
