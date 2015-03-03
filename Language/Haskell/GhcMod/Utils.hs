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
module Language.Haskell.GhcMod.Utils (
    module Language.Haskell.GhcMod.Utils
  , module Utils
  , readProcess
  ) where

import Control.Arrow
import Control.Applicative
import Data.Char
import Language.Haskell.GhcMod.Error
import Exception
import System.Directory (getCurrentDirectory, setCurrentDirectory, doesFileExist)
import System.Process (readProcess)
import System.Directory (getTemporaryDirectory)
import System.FilePath (splitDrive, takeDirectory, takeFileName, pathSeparators,
                        (</>))
import System.IO.Temp (createTempDirectory)
import System.Environment
import Text.Printf

import Paths_ghc_mod (getLibexecDir)
import Utils

-- dropWhileEnd is not provided prior to base 4.5.0.0.
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []

extractParens :: String -> String
extractParens str = extractParens' str 0
 where
   extractParens' :: String -> Int -> String
   extractParens' [] _ = []
   extractParens' (s:ss) level
       | s `elem` "([{" = s : extractParens' ss (level+1)
       | level == 0 = extractParens' ss 0
       | s `elem` "}])" && level == 1 = [s]
       | s `elem` "}])" = s : extractParens' ss (level-1)
       | otherwise = s : extractParens' ss level

withDirectory_ :: (MonadIO m, ExceptionMonad m) => FilePath -> m a -> m a
withDirectory_ dir action =
    gbracket (liftIO getCurrentDirectory) (liftIO . setCurrentDirectory)
                (\_ -> liftIO (setCurrentDirectory dir) >> action)

uniqTempDirName :: FilePath -> FilePath
uniqTempDirName dir = ("ghc-mod"++) $ uncurry (++)
        $ map escapeDriveChar *** map escapePathChar
        $ splitDrive dir
 where
    escapeDriveChar c
        | isAlphaNum c = c
        | otherwise = '-'

    escapePathChar c
        | c `elem` pathSeparators = '-'
        | otherwise = c

newTempDir :: FilePath -> IO FilePath
newTempDir dir =
    flip createTempDirectory (uniqTempDirName dir) =<< getTemporaryDirectory

whenM :: IO Bool -> IO () -> IO ()
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
  exe <- getExecutablePath
  dir <- case takeFileName exe of
    "ghc" -> do -- we're probably in ghci; try CWD
        getCurrentDirectory
    _ ->
        return $ (!!4) $ iterate takeDirectory exe
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
