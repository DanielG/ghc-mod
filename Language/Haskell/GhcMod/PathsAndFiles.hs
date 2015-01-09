{-# LANGUAGE BangPatterns, TupleSections #-}
module Language.Haskell.GhcMod.PathsAndFiles where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Char
import Data.Maybe
import Data.Traversable (traverse)
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
findCabalFile directory = do
    -- Look for cabal files in @dir@ and all it's parent directories
    dcs <- getCabalFiles `zipMapM` parents directory
    -- Extract first non-empty list, which represents a directory with cabal
    -- files.
    case find (not . null) $ uncurry appendDir `map` dcs of
      Just []          -> throw $ GMENoCabalFile
      Just cfs@(_:_:_) -> throw $ GMETooManyCabalFiles cfs
      a  -> return $ head <$> a
 where
   appendDir :: DirPath -> [FileName] -> [FilePath]
   appendDir dir fs = (dir </>) `map` fs

-- | @getCabalFiles dir@. Find all files ending in @.cabal@ in @dir@.
getCabalFiles :: DirPath -> IO [FileName]
getCabalFiles dir =
    filterM isCabalFile =<< getDirectoryContents dir
 where
   isCabalFile f = do
     exists <- doesFileExist $ dir </> f
     return (exists && takeExtension' f == ".cabal")

   takeExtension' p = if takeFileName p == takeExtension p
                        then ""
                        else takeExtension p

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
             -> IO (Maybe FilePath)
getSandboxDb d = do
  mConf <- traverse readFile =<< msum <$>
           sequence [U.mightExist (dir </> "cabal.sandbox.config")
                    | dir <- parents d]
  return $ extractSandboxDbDir =<< mConf

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

packageCache :: String
packageCache = "package.cache"
