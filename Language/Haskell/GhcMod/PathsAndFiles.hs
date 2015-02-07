{-# LANGUAGE BangPatterns, TupleSections #-}
module Language.Haskell.GhcMod.PathsAndFiles where

import Config (cProjectVersion)
import Control.Applicative
import Control.Monad
import Data.List
import Data.Char
import Data.Maybe
import Data.Traversable (traverse)
import Distribution.System (buildPlatform)
import Distribution.Text (display)
import Language.Haskell.GhcMod.Types
import System.Directory
import System.FilePath

import Language.Haskell.GhcMod.Error
import qualified Language.Haskell.GhcMod.Utils as U

import Distribution.Simple.BuildPaths (defaultDistPref)
import Distribution.Simple.Configure (localBuildInfoFile)

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
findCabalFile dir = do
    dcs <- findFileInParentsP  isCabalFile dir
    -- Extract first non-empty list, which represents a directory with cabal
    -- files.
    case find (not . null) $ uncurry appendDir `map` dcs of
      Just []          -> throw $ GMENoCabalFile
      Just cfs@(_:_:_) -> throw $ GMETooManyCabalFiles cfs
      a  -> return $ head <$> a

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
findFileInParentsP p dir =
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
   isSandboxConfig = (=="cabal.sandbox.config")

appendDir :: DirPath -> [FileName] -> [FilePath]
appendDir d fs = (d </>) `map` fs

zipMapM :: Monad m => (a -> m c) -> [a] -> m [(a,c)]
zipMapM f as = mapM (\a -> liftM (a,) $ f a) as

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

-- | Get path to sandbox config file
getSandboxDb :: FilePath -- ^ Path to the cabal package root directory
                         -- (containing the @cabal.sandbox.config@ file)
             -> IO (Maybe GhcPkgDb)
getSandboxDb d = do
  mConf <- traverse readFile =<< U.mightExist (d </> "cabal.sandbox.config")
  return $ PackageDb . fixPkgDbVer <$> (extractSandboxDbDir =<< mConf)

 where
   fixPkgDbVer dir =
       case takeFileName dir == ghcSandboxPkgDbDir of
         True -> dir
         False -> takeDirectory dir </> ghcSandboxPkgDbDir

-- | Extract the sandbox package db directory from the cabal.sandbox.config file.
--   Exception is thrown if the sandbox config file is broken.
extractSandboxDbDir :: String -> Maybe FilePath
extractSandboxDbDir conf = extractValue <$> parse conf
  where
    key = "package-db:"
    keyLen = length key

    parse = listToMaybe . filter (key `isPrefixOf`) . lines
    extractValue = U.dropWhileEnd isSpace . dropWhile isSpace . drop keyLen

setupConfigFile :: Cradle -> FilePath
setupConfigFile crdl = cradleRootDir crdl </> setupConfigPath

-- | Path to 'LocalBuildInfo' file, usually @dist/setup-config@
setupConfigPath :: FilePath
setupConfigPath = localBuildInfoFile defaultDistPref

ghcSandboxPkgDbDir :: String
ghcSandboxPkgDbDir =
   targetPlatform ++ "-ghc-" ++ cProjectVersion ++ "-packages.conf.d"
  where
    targetPlatform = display buildPlatform

packageCache :: String
packageCache = "package.cache"

-- | Filename of the show'ed Cabal setup-config cache
prettyConfigCache :: FilePath
prettyConfigCache = setupConfigPath <.> "ghc-mod-0.pretty-cabal-cache"

-- | Filename of the symbol table cache file.
symbolCache :: Cradle -> FilePath
symbolCache crdl = cradleTempDir crdl </> symbolCacheFile

symbolCacheFile :: String
symbolCacheFile = "ghc-mod-0.symbol-cache"
