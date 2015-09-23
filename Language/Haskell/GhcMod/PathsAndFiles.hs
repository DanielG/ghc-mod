-- ghc-mod: Making Haskell development *more* fun
-- Copyright (C) 2015  Daniel Gröber <dxld ÄT darkboxed DOT org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Language.Haskell.GhcMod.PathsAndFiles (
    module Language.Haskell.GhcMod.PathsAndFiles
  , module Language.Haskell.GhcMod.Caching
  ) where

import Config (cProjectVersion)
import Control.Arrow (second)
import Control.Applicative
import Control.Exception as E
import Control.Monad
import Data.List
import Data.Char
import Data.Maybe
import Data.Traversable hiding (mapM)
import Distribution.Helper (buildPlatform)
import System.Directory
import System.FilePath
import System.Process

import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Caching
import qualified Language.Haskell.GhcMod.Utils as U
import Utils (mightExist)
import Prelude

-- | Guaranteed to be a path to a directory with no trailing slash.
type DirPath = FilePath

-- | Guaranteed to be the name of a file only (no slashes).
type FileName = String

newtype UnString = UnString { unString :: String }

instance Show UnString where
    show = unString

instance Read UnString where
    readsPrec _ = \str -> [(UnString str, "")]

-- | @findCabalFiles dir@. Searches for a @.cabal@ files in @dir@'s parent
-- directories. The first parent directory containing more than one cabal file
-- is assumed to be the project directory. If only one cabal file exists in this
-- directory it is returned otherwise @findCabalFiles@ throws 'GMENoCabalFile'
-- or 'GMETooManyCabalFiles'
findCabalFile :: FilePath -> IO (Maybe FilePath)
findCabalFile dir = do
    -- List of directories and all cabal file candidates
    dcs <- findFileInParentsP  isCabalFile dir :: IO ([(DirPath, [FileName])])
    let css = uncurry appendDir `map` dcs :: [[FilePath]]
    case find (not . null) css of
      Nothing -> return Nothing
      Just cfs@(_:_:_) -> throw $ GMETooManyCabalFiles cfs
      Just (a:_)       -> return (Just a)
      Just []          -> error "findCabalFile"
 where
   appendDir :: DirPath -> [FileName] -> [FilePath]
   appendDir d fs = (d </>) `map` fs

findStackConfigFile :: FilePath -> IO (Maybe FilePath)
findStackConfigFile dir = do
    fs <- map (second listToMaybe) <$> findFileInParentsP  (=="stack.yaml") dir
    case find (isJust . snd) fs of
      Nothing           -> return Nothing
      Just (d, Just a)  -> return $ Just $ d </> a
      Just (_, Nothing) -> error "findStackConfigFile"

-- | Get path to sandbox config file
getSandboxDb :: Cradle -> IO (Maybe GhcPkgDb)
getSandboxDb crdl = do
  mConf <-traverse readFile =<< mightExist (sandboxConfigFile crdl)
  bp <- buildPlatform readProcess
  return $ PackageDb . fixPkgDbVer bp <$> (extractSandboxDbDir =<< mConf)

 where
   fixPkgDbVer bp dir =
       case takeFileName dir == ghcSandboxPkgDbDir bp of
         True -> dir
         False -> takeDirectory dir </> ghcSandboxPkgDbDir bp

-- | Extract the sandbox package db directory from the cabal.sandbox.config
-- file. Exception is thrown if the sandbox config file is broken.
extractSandboxDbDir :: String -> Maybe FilePath
extractSandboxDbDir conf = extractValue <$> parse conf
  where
    key = "package-db:"
    keyLen = length key

    parse = listToMaybe . filter (key `isPrefixOf`) . lines
    extractValue = U.dropWhileEnd isSpace . dropWhile isSpace . drop keyLen


-- |
-- >>> isCabalFile "/home/user/.cabal"
-- False
isCabalFile :: FilePath -> Bool
isCabalFile f = takeExtension' f == ".cabal"

-- |
-- >>> takeExtension' "/some/dir/bla.cabal"
-- ".cabal"
--
-- >>> takeExtension' "some/reldir/bla.cabal"
-- ".cabal"
--
-- >>> takeExtension' "bla.cabal"
-- ".cabal"
--
-- >>> takeExtension' ".cabal"
-- ""
takeExtension' :: FilePath -> String
takeExtension' p =
    if takeFileName p == takeExtension p
      then "" -- just ".cabal" is not a valid cabal file
      else takeExtension p

-- | @findFileInParentsP p dir@ Look for files satisfying @p@ in @dir@ and all
-- it's parent directories.
findFileInParentsP :: (FilePath -> Bool) -> FilePath
                   -> IO [(DirPath, [FileName])]
findFileInParentsP p dir' = U.makeAbsolute' dir' >>= \dir ->
    getFilesP p `zipMapM` parents dir

-- | @getFilesP p dir@. Find all __files__ satisfying @p@ in @.cabal@ in @dir@.
getFilesP :: (FilePath -> Bool) -> DirPath -> IO [FileName]
getFilesP p dir = filterM p' =<< getDirectoryContents dir
 where
   p' fn = do
     (p fn && ) <$> doesFileExist (dir </> fn)

findCabalSandboxDir :: FilePath -> IO (Maybe FilePath)
findCabalSandboxDir dir = do
  dss <- findFileInParentsP isSandboxConfig dir
  return $ case find (not . null . snd) $ dss of
             Just (sbDir, _:_) -> Just sbDir
             _ -> Nothing

 where
   isSandboxConfig = (==sandboxConfigFileName)

zipMapM :: Monad m => (a -> m c) -> [a] -> m [(a,c)]
zipMapM f as = mapM (\a -> liftM ((,) a) $ f a) as

-- | @parents dir@. Returns all parent directories of @dir@ including @dir@.
--
-- Examples
--
-- >>> parents "foo"
-- ["foo"]
--
-- >>> parents "/foo"
-- ["/foo","/"]
--
-- >>> parents "/foo/bar"
-- ["/foo/bar","/foo","/"]
--
-- >>> parents "foo/bar"
-- ["foo/bar","foo"]
parents :: FilePath -> [FilePath]
parents "" = []
parents dir' =
    let (drive, dir) = splitDrive $ normalise $ dropTrailingPathSeparator dir'
    in map (joinDrive drive) $ parents' $ filter (/=".") $ splitDirectories dir
 where
   parents' :: [String] -> [FilePath]
   parents' [] | isAbsolute dir' = "":[]
   parents' [] = []
   parents' dir = [joinPath dir] ++ parents' (init dir)

----------------------------------------------------------------

setupConfigFile :: Cradle -> FilePath
setupConfigFile crdl =
    cradleRootDir crdl </> setupConfigPath (cradleDistDir crdl)

sandboxConfigFile :: Cradle -> FilePath
sandboxConfigFile crdl = cradleRootDir crdl </> sandboxConfigFileName

sandboxConfigFileName :: String
sandboxConfigFileName = "cabal.sandbox.config"

-- | Path to 'LocalBuildInfo' file, usually @dist/setup-config@
setupConfigPath :: FilePath -> FilePath
setupConfigPath dist = dist </> "setup-config"
 -- localBuildInfoFile defaultDistPref

macrosHeaderPath :: FilePath
macrosHeaderPath = autogenModulesDir </> "cabal_macros.h"

autogenModulePath :: String -> String
autogenModulePath pkg_name =
    autogenModulesDir </> ("Paths_" ++ map fixchar pkg_name) <.> ".hs"
  where fixchar '-' = '_'
        fixchar c   = c

autogenModulesDir :: FilePath
autogenModulesDir = "build" </> "autogen"

ghcSandboxPkgDbDir :: String -> String
ghcSandboxPkgDbDir buildPlatf = do
    buildPlatf ++ "-ghc-" ++ cProjectVersion ++ "-packages.conf.d"

packageCache :: String
packageCache = "package.cache"

-- | Filename of the symbol table cache file.
symbolCache :: Cradle -> FilePath
symbolCache crdl = cradleTempDir crdl </> symbolCacheFile

symbolCacheFile :: String
symbolCacheFile = "ghc-mod.symbol-cache"

resolvedComponentsCacheFile :: FilePath -> FilePath
resolvedComponentsCacheFile dist =
    setupConfigPath dist <.> "ghc-mod.resolved-components"

cabalHelperCacheFile :: FilePath -> FilePath
cabalHelperCacheFile dist =
    setupConfigPath dist <.> "ghc-mod.cabal-components"

mergedPkgOptsCacheFile :: FilePath -> FilePath
mergedPkgOptsCacheFile dist =
    setupConfigPath dist <.> "ghc-mod.package-options"

pkgDbStackCacheFile :: FilePath -> FilePath
pkgDbStackCacheFile dist =
    setupConfigPath dist <.> "ghc-mod.package-db-stack"

-- | @findCustomPackageDbFile dir@. Searches for a @ghc-mod.package-db-stack@ file in @dir@.
-- If it exists in the given directory it is returned otherwise @findCradleFile@
-- returns @Nothing@
findCustomPackageDbFile :: FilePath -> IO (Maybe FilePath)
findCustomPackageDbFile directory = do
    let path = directory </> "ghc-mod.package-db-stack"
    mightExist path
