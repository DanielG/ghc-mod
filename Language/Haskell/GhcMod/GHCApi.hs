{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

module Language.Haskell.GhcMod.GHCApi (
    withGHC
  , withGHC'
  , initializeFlagsWithCradle
  , setTargetFiles
  , addTargetFiles
  , getDynamicFlags
  , getSystemLibDir
  , withDynFlags
  ) where

import Language.Haskell.GhcMod.CabalApi
import Language.Haskell.GhcMod.ErrMsg
import Language.Haskell.GhcMod.GHCChoice
import Language.Haskell.GhcMod.GhcPkg

import Control.Applicative ((<$>))
import Control.Monad (forM, void)
import CoreMonad (liftIO)
import Data.Maybe (isJust, fromJust)
import Distribution.PackageDescription (PackageDescription)
import DynFlags (dopt_set)
import Exception (ghandle, SomeException(..))
import GHC (Ghc, GhcMonad, DynFlags(..), GhcLink(..), HscTarget(..), LoadHowMuch(..))
import qualified GHC as G
import qualified Language.Haskell.GhcMod.Gap as Gap
import Language.Haskell.GhcMod.Types
import System.Exit (exitSuccess)
import System.IO (hPutStr, hPrint, stderr)
import System.Process (readProcess)

----------------------------------------------------------------

-- | Obtaining the directory for system libraries.
getSystemLibDir :: IO (Maybe FilePath)
getSystemLibDir = do
    res <- readProcess "ghc" ["--print-libdir"] []
    return $ case res of
        ""   -> Nothing
        dirn -> Just (init dirn)

----------------------------------------------------------------

-- | Converting the 'Ghc' monad to the 'IO' monad.
withGHC :: FilePath  -- ^ A target file displayed in an error message.
        -> Ghc a -- ^ 'Ghc' actions created by the Ghc utilities.
        -> IO a
withGHC file body = ghandle ignore $ withGHC' body
  where
    ignore :: SomeException -> IO a
    ignore e = do
        hPutStr stderr $ file ++ ":0:0:Error:"
        hPrint stderr e
        exitSuccess

withGHC' :: Ghc a -> IO a
withGHC' body = do
    mlibdir <- getSystemLibDir
    G.runGhc mlibdir $ do
        dflags <- G.getSessionDynFlags
        G.defaultCleanupHandler dflags body

----------------------------------------------------------------

importDirs :: [IncludeDir]
importDirs = [".","..","../..","../../..","../../../..","../../../../.."]

data Build = CabalPkg | SingleFile deriving Eq

-- | Initialize the 'DynFlags' relating to the compilation of a single
-- file or GHC session according to the 'Cradle' and 'Options'
-- provided.
initializeFlagsWithCradle :: GhcMonad m
        => Options
        -> Cradle
        -> [GHCOption]
        -> Bool
        -> m (LogReader, Maybe PackageDescription)
initializeFlagsWithCradle opt cradle ghcopts logging
  | cabal     = withCabal |||> withSandbox
  | otherwise = withSandbox
  where
    mCradleFile = cradleCabalFile cradle
    cabal = isJust mCradleFile
    withCabal = do
        pkgDesc <- liftIO $ parseCabalFile $ fromJust mCradleFile
        compOpts <- liftIO $ getCompilerOptions ghcopts cradle pkgDesc
        logger <- initSession CabalPkg opt compOpts logging
        return (logger, Just pkgDesc)
    withSandbox = do
        logger <- initSession SingleFile opt compOpts logging
        return (logger, Nothing)
      where
        pkgOpts = ghcDbStackOpts $ cradlePkgDbStack cradle
        compOpts
          | null pkgOpts = CompilerOptions ghcopts importDirs []
          | otherwise    = CompilerOptions (ghcopts ++ pkgOpts) [wdir,rdir] []
        wdir = cradleCurrentDir cradle
        rdir = cradleRootDir    cradle

----------------------------------------------------------------

initSession :: GhcMonad m
            => Build
            -> Options
            -> CompilerOptions
            -> Bool
            -> m LogReader
initSession build opt compOpts logging = do
    df <- initDynFlags build opt compOpts
    (df', lg) <- liftIO $ setLogger logging df opt
    _ <- G.setSessionDynFlags df'
    return lg

initDynFlags :: GhcMonad m => Build -> Options -> CompilerOptions -> m DynFlags
initDynFlags build Options {..} CompilerOptions {..} = do
    df <- G.getSessionDynFlags
    _ <- G.setSessionDynFlags =<< (addCmdOpts ghcOptions
      $ setLinkerOptions
      $ setIncludeDirs includeDirs
      $ setSplice expandSplice
      $ setBuildEnv build
      $ Gap.addPackageFlags depPackages df)
    G.getSessionDynFlags


----------------------------------------------------------------

-- we don't want to generate object code so we compile to bytecode
-- (HscInterpreted) which implies LinkInMemory
-- HscInterpreted
setLinkerOptions :: DynFlags -> DynFlags
setLinkerOptions df = df {
    ghcLink   = LinkInMemory
  , hscTarget = HscInterpreted
  }

setIncludeDirs :: [IncludeDir] -> DynFlags -> DynFlags
setIncludeDirs idirs df = df { importPaths = idirs }

setBuildEnv :: Build -> DynFlags -> DynFlags
setBuildEnv build = setHideAllPackages build . setCabalPackage build

-- | Set option in 'DynFlags' to Expand template haskell if first argument is
-- True
setSplice :: Bool -> DynFlags -> DynFlags
setSplice False df = df
setSplice True df  = dopt_set df Gap.dumpSplicesFlag

-- At the moment with this option set ghc only prints different error messages,
-- suggesting the user to add a hidden package to the build-depends in his cabal
-- file for example
setCabalPackage :: Build -> DynFlags -> DynFlags
setCabalPackage CabalPkg df = Gap.setCabalPkg df
setCabalPackage _ df = df

-- | Enable hiding of all package not explicitly exposed (like Cabal does)
setHideAllPackages :: Build -> DynFlags -> DynFlags
setHideAllPackages CabalPkg df = Gap.setHideAllPackages df
setHideAllPackages _ df = df

-- | Parse command line ghc options and add them to the 'DynFlags' passed
addCmdOpts :: GhcMonad m => [GHCOption] -> DynFlags -> m DynFlags
addCmdOpts cmdOpts df =
    tfst <$> G.parseDynamicFlags df (map G.noLoc cmdOpts)
  where
    tfst (a,_,_) = a

----------------------------------------------------------------

-- | Set the files and load
setTargetFiles :: (GhcMonad m) => [FilePath] -> m ()
setTargetFiles [] = error "ghc-mod: setTargetFiles: No target files given"
setTargetFiles files = do
    targets <- forM files $ \file -> G.guessTarget file Nothing
    G.setTargets targets
    void $ G.load LoadAllTargets

-- | Adding the files to the targets.
addTargetFiles :: (GhcMonad m) => [FilePath] -> m ()
addTargetFiles [] = error "ghc-mod: addTargetFiles: No target files given"
addTargetFiles files = do
    targets <- forM files $ \file -> G.guessTarget file Nothing
    mapM_ G.addTarget targets

----------------------------------------------------------------

-- | Return the 'DynFlags' currently in use in the GHC session.
getDynamicFlags :: IO DynFlags
getDynamicFlags = do
    mlibdir <- getSystemLibDir
    G.runGhc mlibdir G.getSessionDynFlags

withDynFlags :: (DynFlags -> DynFlags) -> Ghc a -> Ghc a
withDynFlags setFlag body = G.gbracket setup teardown (\_ -> body)
  where
    setup = do
        dflag <- G.getSessionDynFlags
        void $ G.setSessionDynFlags (setFlag dflag)
        return dflag
    teardown = void . G.setSessionDynFlags
