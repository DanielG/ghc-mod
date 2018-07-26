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

{-# LANGUAGE CPP #-}
{-# LANGUAGE DoAndIfThenElse #-}

module GhcMod.Utils (
    module GhcMod.Utils
  , module Utils
  , readProcess
  ) where

import Control.Applicative
import Data.Char
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Either (rights)
import Data.List (inits)
import Exception
import GhcMod.Error
import GhcMod.Types
import GhcMod.Monad.Types
import System.Directory
import System.Environment
import System.FilePath
import System.IO.Temp (createTempDirectory)
import System.Process (readProcess)
import Text.Printf

import Paths_ghc_mod_core (getLibexecDir, getBinDir)
import Utils
import Prelude

-- dropWhileEnd is not provided prior to base 4.5.0.0.
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []

withDirectory_ :: (MonadIO m, ExceptionMonad m) => FilePath -> m a -> m a
withDirectory_ dir action =
  gbracket
    (liftIO getCurrentDirectory)
    (liftIO . setCurrentDirectory)
    (\_ -> liftIO (setCurrentDirectory dir) >> action)

uniqTempDirName :: FilePath -> FilePath
uniqTempDirName dir =
  "ghc-mod" ++ map escapeDriveChar drive ++ map escapePathChar path
  where
    (drive, path) = splitDrive dir
    escapeDriveChar :: Char -> Char
    escapeDriveChar c
      | isAlphaNum c = c
      | otherwise     = '-'
    escapePathChar :: Char -> Char
    escapePathChar c
      | c `elem` pathSeparators = '-'
      | otherwise               = c

newTempDir :: FilePath -> IO FilePath
newTempDir _dir =
  flip createTempDirectory "ghc-mod" =<< getTemporaryDirectory

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb ma = mb >>= flip when ma

-- | Returns the path to the currently running ghc-mod executable. With ghc<7.6
-- this is a guess but >=7.6 uses 'getExecutablePath'.
ghcModExecutable :: IO FilePath
ghcModExecutable = do
    exe <- getExecutablePath'
    stack <- lookupEnv "STACK_EXE"
    case takeBaseName exe of
      "spec" | Just _ <- stack ->
          (</> "ghc-mod") <$> getBinDir
      "spec" ->
          (</> "dist/build/ghc-mod/ghc-mod") <$> getCurrentDirectory
      "ghc-mod" ->
          return exe
      _ ->
          return $ takeDirectory exe </> "ghc-mod"

getExecutablePath' :: IO FilePath
#if __GLASGOW_HASKELL__ >= 706
getExecutablePath' = getExecutablePath
#else
getExecutablePath' = getProgName
#endif

canonFilePath :: FilePath -> IO FilePath
canonFilePath f = do
  p <- canonicalizePath f
  e <- doesFileExist p
  when (not e) $ error $ "canonFilePath: not a file: " ++ p
  return p

withMappedFile :: (IOish m, GmState m, GmEnv m) =>
                  forall a. FilePath -> (FilePath -> m a) -> m a
withMappedFile file action = getCanonicalFileNameSafe file >>= lookupMMappedFile >>= runWithFile
  where
    runWithFile (Just to) = action $ fmPath to
    runWithFile _ = action file

getCanonicalFileNameSafe :: (IOish m) => FilePath -> m FilePath
getCanonicalFileNameSafe fn = do
  let fn' = normalise fn
  pl <- liftIO $ rights <$> (mapM ((try :: IO FilePath -> IO (Either SomeException FilePath)) . canonicalizePath . joinPath) $ reverse $ inits $ splitPath' fn')
  return $
    if (length pl > 0)
    then joinPath $ (head pl):(drop (length pl - 1) (splitPath fn'))
    else error "Current dir doesn't seem to exist?"
  where
#if __GLASGOW_HASKELL__ < 710
    splitPath' = (".":) . splitPath
#else
    splitPath' = splitPath
#endif

mkRevRedirMapFunc :: (Functor m, GmState m, GmEnv m) => m (FilePath -> FilePath)
mkRevRedirMapFunc = do
  rm <- M.fromList <$> map (uncurry mf) <$> M.toList <$> getMMappedFiles
  crdl <- cradle
  return $ \key ->
    fromMaybe key
    $ makeRelative (cradleRootDir crdl)
    <$> M.lookup key rm
  where
    mf :: FilePath -> FileMapping -> (FilePath, FilePath)
    mf from to = (fmPath to, from)

findFilesWith' :: (FilePath -> IO Bool) -> [FilePath] -> String -> IO [FilePath]
findFilesWith' _ [] _ = return []
findFilesWith' f (d:ds) fileName = do
    let file = d </> fileName
    exist <- doesFileExist file
    b <- if exist then f file else return False
    if b then do
               files <- findFilesWith' f ds fileName
               return $ file : files
        else findFilesWith' f ds fileName


-- Copyright   :  (c) The University of Glasgow 2001
-- | Make a path absolute by prepending the current directory (if it isn't
-- already absolute) and applying 'normalise' to the result.
--
-- If the path is already absolute, the operation never fails.  Otherwise, the
-- operation may fail with the same exceptions as 'getCurrentDirectory'.
makeAbsolute' :: FilePath -> IO FilePath
makeAbsolute' = (normalise <$>) . absolutize
  where absolutize path -- avoid the call to `getCurrentDirectory` if we can
          | isRelative path = (</> path) <$> getCurrentDirectory
          | otherwise       = return path
