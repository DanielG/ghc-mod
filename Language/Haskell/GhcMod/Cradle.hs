module Language.Haskell.GhcMod.Cradle (findCradle) where

import Control.Applicative ((<$>))
import Control.Exception (throwIO)
import Control.Monad (unless, filterM)
import Data.List (isSuffixOf)
import Distribution.System (buildPlatform)
import qualified Distribution.Text as Text (display)
import Language.Haskell.GhcMod.Types
import System.Directory (getCurrentDirectory, getDirectoryContents, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>),takeDirectory)

----------------------------------------------------------------

-- | Finding 'Cradle'.
--   An error would be thrown.
findCradle :: Maybe FilePath -- ^ A 'FilePath' for a sandbox.
           -> GHCVersion
           -> IO Cradle
findCradle (Just sbox) strver = do
    (pkgConf,exist) <- checkPackageConf sbox strver
    unless exist $  throwIO $ userError $ pkgConf ++ " not found"
    wdir <- getCurrentDirectory
    cfiles <- cabalDir wdir
    return $ case cfiles of
        Nothing -> Cradle {
            cradleCurrentDir  = wdir
          , cradleCabalDir    = Nothing
          , cradleCabalFile   = Nothing
          , cradlePackageConf = Just pkgConf
          }
        Just (cdir,cfile,_) -> Cradle {
            cradleCurrentDir  = wdir
          , cradleCabalDir    = Just cdir
          , cradleCabalFile   = Just cfile
          , cradlePackageConf = Just pkgConf
          }
findCradle Nothing strver = do
    wdir <- getCurrentDirectory
    cfiles <- cabalDir wdir
    case cfiles of
        Nothing -> return Cradle {
            cradleCurrentDir  = wdir
          , cradleCabalDir    = Nothing
          , cradleCabalFile   = Nothing
          , cradlePackageConf = Nothing
          }
        Just (cdir,cfile,Nothing) -> do
            return Cradle {
                cradleCurrentDir  = wdir
              , cradleCabalDir    = Just cdir
              , cradleCabalFile   = Just cfile
              , cradlePackageConf = Nothing
              }
        Just (cdir,cfile,Just sbox) -> do
            (pkgConf,exist) <- checkPackageConf sbox strver
            return Cradle {
                cradleCurrentDir  = wdir
              , cradleCabalDir    = Just cdir
              , cradleCabalFile   = Just cfile
              , cradlePackageConf = if exist then Just pkgConf else Nothing
              }

----------------------------------------------------------------

cabalSuffix :: String
cabalSuffix = ".cabal"

cabalSuffixLength :: Int
cabalSuffixLength = length cabalSuffix

-- Finding a Cabal file up to the root directory
-- Input: a directly to investigate
-- Output: (the path to the directory containing a Cabal file
--         ,the path to the Cabal file
--         ,Just the path to the sandbox directory)
cabalDir :: FilePath -> IO (Maybe (FilePath,FilePath,Maybe FilePath))
cabalDir dir = do
    cnts <- (filter isCabal <$> getDirectoryContents dir)
            >>= filterM (\file -> doesFileExist (dir </> file))
    let dir' = takeDirectory dir
    case cnts of
        [] | dir' == dir -> return Nothing
           | otherwise   -> cabalDir dir'
        cfile:_          -> do
            msbox <- checkSandbox dir
            return $ Just (dir,dir </> cfile, msbox)
  where
    isCabal name = cabalSuffix `isSuffixOf` name
                && length name > cabalSuffixLength

----------------------------------------------------------------

sandboxConfig :: String
sandboxConfig = "cabal.sandbox.config"

sandboxDir :: String
sandboxDir = ".cabal-sandbox"

checkSandbox :: FilePath -> IO (Maybe FilePath)
checkSandbox dir = do
    let conf = dir </> sandboxConfig
        sbox = dir </> sandboxDir
    sandboxConfigExists <- doesFileExist conf
    sandboxExists <- doesDirectoryExist sbox
    if sandboxConfigExists && sandboxExists then
        return (Just sbox)
      else
        return Nothing

----------------------------------------------------------------

packageConfName :: GHCVersion -> FilePath
packageConfName strver = Text.display buildPlatform
                      ++ "-ghc-"
                      ++ strver
                      ++ "-packages.conf.d"

checkPackageConf :: FilePath -> GHCVersion -> IO (FilePath, Bool)
checkPackageConf path strver = do
    let dir = path </> packageConfName strver
    exist <- doesDirectoryExist dir
    return (dir,exist)
