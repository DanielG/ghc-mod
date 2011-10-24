{-
Find, if it exists, a directory with the name 'cabal-dev/packages-X.X.X.conf'
the path of which we have to add to the GHC options that ghc-mod uses.
where X.X.X is the GHC version.
-}

module CabalDev where

import Maybe                  (listToMaybe)
import System.FilePath.Find
import System.FilePath.Posix (splitPath,joinPath)
import System.Posix.Directory (getWorkingDirectory)

import Types

modify_options :: Options -> IO Options
modify_options opts =
  fmap (maybe opts (add_path opts)) find_cabal_dev

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
