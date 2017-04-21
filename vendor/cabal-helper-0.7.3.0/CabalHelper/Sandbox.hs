module CabalHelper.Sandbox where

import CabalHelper.Common
import Control.Applicative
import Data.Char
import Data.Maybe
import Data.List
import Distribution.Version
import System.FilePath
import System.Directory
import Prelude

import qualified Data.Traversable as T

-- | Get the path to the sandbox package-db in a project
getSandboxPkgDb :: FilePath
             -- ^ Path to the cabal package root directory (containing the
             -- @cabal.sandbox.config@ file)
             -> String
             -- ^ Cabal build platform, i.e. @buildPlatform@
             -> Version
             -- ^ GHC version (@cProjectVersion@ is your friend)
             -> IO (Maybe FilePath)
getSandboxPkgDb d platform ghcVer = do
  mConf <- T.traverse readFile =<< mightExist (d </> "cabal.sandbox.config")
  return $ fixPkgDbVer <$> (extractSandboxDbDir =<< mConf)

 where
   fixPkgDbVer dir =
       case takeFileName dir == ghcSandboxPkgDbDir platform ghcVer of
         True -> dir
         False -> takeDirectory dir </> ghcSandboxPkgDbDir platform ghcVer

ghcSandboxPkgDbDir :: String -> Version -> String
ghcSandboxPkgDbDir platform ghcVer =
   platform ++ "-ghc-" ++ showVersion ghcVer ++ "-packages.conf.d"

-- | Extract the sandbox package db directory from the cabal.sandbox.config
-- file. Exception is thrown if the sandbox config file is broken.
extractSandboxDbDir :: String -> Maybe FilePath
extractSandboxDbDir conf = extractValue <$> parse conf
  where
    key = "package-db:"
    keyLen = length key

    parse = listToMaybe . filter (key `isPrefixOf`) . lines
    extractValue = CabalHelper.Sandbox.dropWhileEnd isSpace . dropWhile isSpace . drop keyLen


mightExist :: FilePath -> IO (Maybe FilePath)
mightExist f = do
  exists <- doesFileExist f
  return $ if exists then (Just f) else (Nothing)

-- dropWhileEnd is not provided prior to base 4.5.0.0.
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []
