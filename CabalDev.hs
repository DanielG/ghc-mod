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
    found = addPath opts <$> findCabalDev
    notFound = return opts

findCabalDev :: IO String
findCabalDev = getCurrentDirectory >>= searchIt . splitPath

addPath :: Options -> String -> Options
addPath orig_opts path = do
    let orig_ghcopt = ghcOpts orig_opts
    orig_opts { ghcOpts = orig_ghcopt ++ ["-package-conf", path] }

searchIt :: [FilePath] -> IO FilePath
searchIt [] = throwIO $ userError "Not found"
searchIt path = do
    a <- doesDirectoryExist (mpath path)
    if a then
        findConf (mpath path)
      else
        searchIt $ init path
  where
    mpath a = joinPath a </> "cabal-dev/"

findConf :: FilePath -> IO FilePath
findConf path = do
    Just f <- find (=~ "packages.*\\.conf") <$> getDirectoryContents path
    return $ path </> f
