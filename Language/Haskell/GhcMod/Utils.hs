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

{-# LANGUAGE CPP #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Language.Haskell.GhcMod.Utils (
    module Language.Haskell.GhcMod.Utils
  , module Utils
  , readProcess
  ) where

import Control.Applicative
import Data.Char
import Exception
import Language.Haskell.GhcMod.Error
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Monad.Types
import System.Directory (getCurrentDirectory, setCurrentDirectory, doesFileExist,
                         getTemporaryDirectory, canonicalizePath, removeFile)
import System.Environment
import System.FilePath (splitDrive, takeDirectory, takeFileName, pathSeparators,
                        (</>))
import System.IO.Temp (createTempDirectory, openTempFile)
import System.IO (hPutStr, hClose)
import System.Process (readProcess)
import Text.Printf

import Paths_ghc_mod (getLibexecDir)
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
newTempDir dir =
  flip createTempDirectory (uniqTempDirName dir) =<< getTemporaryDirectory

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb ma = mb >>= flip when ma

-- | Returns the path to the currently running ghc-mod executable. With ghc<7.6
-- this is a guess but >=7.6 uses 'getExecutablePath'.
ghcModExecutable :: IO FilePath
#ifndef SPEC
ghcModExecutable = do
    dir <- takeDirectory <$> getExecutablePath'
    return $ (if dir == "." then "" else dir) </> "ghc-mod"
#else
ghcModExecutable = fmap (</> "dist/build/ghc-mod/ghc-mod") getCurrentDirectory
#endif

findLibexecExe :: String -> IO FilePath
findLibexecExe "cabal-helper-wrapper" = do
  libexecdir <- getLibexecDir
  let exeName = "cabal-helper-wrapper"
      exe = libexecdir </> exeName

  exists <- doesFileExist exe

  if exists
  then return exe
  else do
    mdir <- tryFindGhcModTreeDataDir
    case mdir of
      Nothing ->
        error $ libexecNotExitsError exeName libexecdir
      Just dir ->
        return $ dir </> "dist" </> "build" </> exeName </> exeName
findLibexecExe exe = error $ "findLibexecExe: Unknown executable: " ++ exe

libexecNotExitsError :: String -> FilePath -> String
libexecNotExitsError exe dir = printf
 ( "Could not find $libexecdir/%s\n"
 ++"\n"
 ++"If you are a developer set the environment variable `ghc_mod_libexecdir'\n"
 ++"to override $libexecdir[1] the following will work in the ghc-mod tree:\n"
 ++"\n"
 ++"    $ export ghc_mod_libexecdir=$PWD/dist/build/%s\n"
 ++"\n"
 ++"[1]: %s\n"
 ++"\n"
 ++"If you don't know what I'm talking about something went wrong with your\n"
 ++"installation. Please report this problem here:\n"
 ++"\n"
 ++"    https://github.com/kazu-yamamoto/ghc-mod/issues") exe exe dir

tryFindGhcModTreeLibexecDir :: IO (Maybe FilePath)
tryFindGhcModTreeLibexecDir  = do
  exe <- getExecutablePath'
  dir <- case takeFileName exe of
    "ghc" -> getCurrentDirectory -- we're probably in ghci; try CWD
    _     -> return $ (!!4) $ iterate takeDirectory exe
  exists <- doesFileExist $ dir </> "ghc-mod.cabal"
  return $ if exists
           then Just dir
           else Nothing

tryFindGhcModTreeDataDir :: IO (Maybe FilePath)
tryFindGhcModTreeDataDir  = do
  dir <- (!!4) . iterate takeDirectory <$> getExecutablePath'
  exists <- doesFileExist $ dir </> "ghc-mod.cabal"
  return $ if exists
           then Just dir
           else Nothing

readLibExecProcess' :: (MonadIO m, ExceptionMonad m)
                    => String -> [String] -> m String
readLibExecProcess' cmd args = do
  exe <- liftIO $ findLibexecExe cmd
  liftIO $ readProcess exe args ""

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
withMappedFile file action = lookupMMappedFile file >>= runWithFile
  where
    runWithFile (Just (RedirectedMapping to)) = action to
    runWithFile (Just (MemoryMapping (Just src))) = do
      crdl <- cradle
      (fp,hndl) <- liftIO $ openTempFile (cradleTempDir crdl) (takeFileName file)
      liftIO $ hPutStr hndl src
      liftIO $ hClose hndl
      result <- action fp
      liftIO $ removeFile fp
      return result
    runWithFile _ = action file
