module CabalDev (modifyOptions) where

{-
  If the directory 'cabal-dev/packages-X.X.X.conf' exists, add it to the
  options ghc-mod uses to check the source.  Otherwise just pass it on.
-}

import Control.Applicative ((<$>))
import Control.Exception (throwIO)
import Control.Exception.IOChoice
import Data.List (find)
import System.Directory
import System.FilePath (splitPath,joinPath,(</>))
import Text.Regex.Posix ((=~))
import Types

modifyOptions :: Options -> IO Options
modifyOptions opts = found ||> notFound
  where
    found = addPath opts <$> findCabalDev (sandbox opts)
    notFound = return opts

findCabalDev :: Maybe String -> IO FilePath
findCabalDev Nothing = getCurrentDirectory >>= searchIt . splitPath
findCabalDev (Just path) = do
    exist <- doesDirectoryExist path
    if exist then
        findConf path
      else
        findCabalDev Nothing

addPath :: Options -> String -> Options
addPath orig_opts path = orig_opts { ghcOpts = opts' }
  where
    orig_ghcopt = ghcOpts orig_opts
    opts' = orig_ghcopt ++ ["-package-conf", path, "-no-user-package-conf"]

searchIt :: [FilePath] -> IO FilePath
searchIt [] = throwIO $ userError "Not found"
searchIt path = do
    exist <- doesDirectoryExist cabalDir
    if exist then
        findConf cabalDir
      else
        searchIt $ init path
  where
    cabalDir = mpath path
    mpath a = joinPath a </> "cabal-dev/"

findConf :: FilePath -> IO FilePath
findConf path = do
    Just f <- find (=~ "packages.*\\.conf") <$> getDirectoryContents path
    return $ path </> f
