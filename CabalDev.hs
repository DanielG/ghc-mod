
module CabalDev (modify_options) where

import System.Directory
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

-- Search down the line for a 'cabal-dev' directory
-- and if found, look for the package.conf
searchit :: [FilePath] -> IO (Maybe String)
searchit [] = return Nothing
searchit p = do
  xx <- doesDirectoryExist $ (joinPath p) ++"/cabal-dev"
  case xx of
    False -> searchit $ init p
    True -> return $ Just $ make_mod_path p
 where
   make_mod_path :: [FilePath] -> FilePath
   make_mod_path x = do
     let path = joinPath x
     case hasTrailingPathSeparator path of
       False -> path ++ "/" ++ pkg_string
       True -> path ++ pkg_string

-- I really need to find a solution for this.
pkg_string :: String
pkg_string =
  "cabal-dev/packages-7.0.4.conf"
