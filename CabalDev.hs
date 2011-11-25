{-# LANGUAGE DoAndIfThenElse #-}

module CabalDev (modifyOptions) where

{-
If the directory 'cabal-dev/packages-X.X.X.conf' exists, add it to the
options ghc-mod uses to check the source.  Otherwise just pass it on.
-}

import Control.Applicative    ((<$>))
import Data.Maybe             (listToMaybe)
import System.FilePath.Find
import System.FilePath.Posix  (splitPath,joinPath,(</>))
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
  let orig_ghcopt = ghcOpts orig_opts
  orig_opts { ghcOpts = orig_ghcopt ++ ["-package-conf", path] }

searchIt :: [FilePath] -> IO (Maybe FilePath)
searchIt [] = return Nothing
searchIt path = do
  a <- doesDirectoryExist (mpath path)
  if a then do
    listToMaybe <$> find always (fileName ~~? "packages*.conf") (mpath path)
  else
    searchIt $ init path
  where
    mpath a = joinPath a </> "cabal-dev/"
