
module CabalDev (find_cabal_dev) where

import System.Directory
import System.Posix.Directory
import System.FilePath.Posix

find_cabal_dev :: IO [FilePath]
find_cabal_dev = do
    pth <- getWorkingDirectory
    lx <- searchit $ splitPath pth
    case lx of
      Nothing -> return []
      Just y -> return [y]
 where
   make_mod_path :: [FilePath] -> FilePath
   make_mod_path x = do
     let tp = joinPath x
     case hasTrailingPathSeparator tp of
       False -> tp ++ "/" ++ pkg_string
       True -> tp ++ pkg_string

   searchit :: [FilePath] -> IO (Maybe String)
   searchit p =
     case (length p) > 1 of
       False -> return Nothing
       True -> do
           xx <- doesDirectoryExist $ (joinPath p) ++"/cabal-dev"
           case xx of
             False -> searchit $ init p
             True -> return $ Just $ make_mod_path p
   
-- Can't seem to find anything on how to get the GHC version.
-- It's late so I'll poke at it some more tomorrow.
   pkg_string :: String
   pkg_string = "cabal-dev/packages-7.0.2.conf"
