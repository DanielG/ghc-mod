-- ghc-mod: Happy Haskell Hacking
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

module GhcMod.PathsAndFiles (
    module GhcMod.PathsAndFiles
  , module GhcMod.Caching
  ) where

import Config (cProjectVersion)
import Control.Arrow (second)
import Control.Applicative
import Control.Exception as E
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.List
import Data.Char
import Data.Maybe
import Data.Traversable hiding (mapM)
import Distribution.Helper (buildPlatform)
import System.Directory
import System.FilePath
import System.Process

import GhcMod.Types
import GhcMod.Caching
import qualified GhcMod.Utils as U
import Utils (mightExist)
import Prelude

-- | Guaranteed to be a path to a directory with no trailing slash.
type DirPath = FilePath

-- | Guaranteed to be the name of a file only (no slashes).
type FileName = String

-- | @findCabalFiles dir@. Searches for a @.cabal@ files in @dir@'s parent
-- directories. The first parent directory containing more than one cabal file
-- is assumed to be the project directory. If only one cabal file exists in this
-- directory it is returned otherwise @findCabalFiles@ throws 'GMENoCabalFile'
-- or 'GMETooManyCabalFiles'
findCabalFile :: FilePath -> IO (Maybe FilePath)
findCabalFile dir = findFileInParentsP isCabalFile pick dir
  where
    pick [] = Nothing
    pick [cf] = Just cf
    pick cfs  = throw $ GMETooManyCabalFiles cfs

findStackConfigFile :: FilePath -> IO (Maybe FilePath)
findStackConfigFile dir =
    findFileInParentsP (=="stack.yaml") pick dir
  where
    pick []     = Nothing
    pick (sf:_) = Just sf

findCabalSandboxDir :: FilePath -> IO (Maybe FilePath)
findCabalSandboxDir dir =
  fmap takeDirectory <$> findFileInParentsP isSandboxConfig pick dir
 where
   isSandboxConfig = (==sandboxConfigFileName)
   pick []     = Nothing
   pick (sc:_) = Just sc

findCustomPackageDbFile :: FilePath -> IO (Maybe FilePath)
findCustomPackageDbFile dir =
    mightExist $ dir </> "ghc-mod.package-db-stack"

-- | Get path to sandbox config file
getSandboxDb :: Cradle -> IO (Maybe GhcPkgDb)
getSandboxDb crdl = do
  mConf <- traverse readFile =<< mightExist (sandboxConfigFile crdl)
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

-- | @findFileInParentsP p r dir@ Look for files satisfying @p@ in @dir@ and all
-- it's parent directories. Files found to satisfy @p@ in a given directory are
-- passed to @r@ and if this yields a 'Just' value the search finishes early
-- without examinig any more directories and this value is returned.
findFileInParentsP :: (FilePath -> Bool)
                   -> ([FilePath] -> Maybe a)
                   -> FilePath
                   -> IO (Maybe a)
findFileInParentsP p r dir = runMaybeT $
    join $ msum <$> map (MaybeT . fmap r) <$> liftIO (findFilesInParentsP p dir)

-- | @findFilesInParentsP p dir@ Look for files satisfying @p@ in @dir@ and all
-- it's parent directories.
findFilesInParentsP :: (FilePath -> Bool) -> FilePath
                   -> IO [IO [FilePath]]
findFilesInParentsP p dir' = U.makeAbsolute' dir' >>= \dir -> return $
    map (\d -> (map (d </>)) <$> getFilesP p d) $ parents dir

-- | @getFilesP p dir@. Find all __files__ satisfying @p@ in @.cabal@ in @dir@.
getFilesP :: (FilePath -> Bool) -> DirPath -> IO [FileName]
getFilesP p dir = filterM p' =<< getDirectoryContentsSafe
 where
   p' fn = do
     (p fn && ) <$> doesFileExist (dir </> fn)
   getDirectoryContentsSafe = do
     rdable <- readable <$> getPermissions dir
     if rdable
        then getDirectoryContents dir
        else return []

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
symbolCache crdl = cradleRootDir crdl </> cradleDistDir crdl </> symbolCacheFile

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
