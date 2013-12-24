{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.GhcMod.GHCApi (
    withGHC
  , withGHCDummyFile
  , initializeFlags
  , initializeFlagsWithCradle
  , setTargetFiles
  , getDynamicFlags
  , setSlowDynFlags
  , checkSlowAndSet
  , canCheckFast
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import CoreMonad
import Data.Maybe (isJust,fromJust)
import Distribution.PackageDescription (PackageDescription)
import DynFlags
import Exception
import GHC
import GHC.Paths (libdir)
import Language.Haskell.GhcMod.CabalApi
import Language.Haskell.GhcMod.ErrMsg
import Language.Haskell.GhcMod.GHCChoice
import qualified Language.Haskell.GhcMod.Gap as Gap
import Language.Haskell.GhcMod.Types
import System.Exit
import System.IO

----------------------------------------------------------------

-- | Converting the 'Ghc' monad to the 'IO' monad.
withGHCDummyFile :: Alternative m => Ghc (m a) -- ^ 'Ghc' actions created by the Ghc utilities.
                                  -> IO (m a)
withGHCDummyFile = withGHC "Dummy"

-- | Converting the 'Ghc' monad to the 'IO' monad.
withGHC :: Alternative m => FilePath  -- ^ A target file displayed in an error message.
                         -> Ghc (m a) -- ^ 'Ghc' actions created by the Ghc utilities.
                         -> IO (m a)
withGHC file body = ghandle ignore $ runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    defaultCleanupHandler dflags body
  where
    ignore :: Alternative m => SomeException -> IO (m a)
    ignore e = do
        hPutStr stderr $ file ++ ":0:0:Error:"
        hPrint stderr e
        exitSuccess

----------------------------------------------------------------

importDirs :: [IncludeDir]
importDirs = [".","..","../..","../../..","../../../..","../../../../.."]

data Build = CabalPkg | SingleFile deriving Eq

-- | Initialize the 'DynFlags' relating to the compilation of a single
-- file or GHC session according to the 'Cradle' and 'Options'
-- provided.
initializeFlagsWithCradle :: GhcMonad m =>  Options -> Cradle -> [GHCOption] -> Bool -> m (LogReader, Maybe PackageDescription)
initializeFlagsWithCradle opt cradle ghcopts logging
  | cabal     = withCabal |||> withoutCabal
  | otherwise = withoutCabal
  where
    mCradleFile = cradleCabalFile cradle
    cabal = isJust mCradleFile
    withCabal = do
        pkgDesc <- liftIO $ parseCabalFile $ fromJust mCradleFile
        compOpts <- liftIO $ getCompilerOptions ghcopts cradle pkgDesc
        logger <- initSession CabalPkg opt compOpts logging
        return (logger, Just pkgDesc)
    withoutCabal = do
        logger <- initSession SingleFile opt compOpts logging
        return (logger, Nothing)
      where
        pkgDbOpts = cradlePackageDbOpts cradle
        compOpts = CompilerOptions (ghcopts ++ pkgDbOpts) importDirs []

----------------------------------------------------------------

initSession :: GhcMonad m => Build
            -> Options
            -> CompilerOptions
            -> Bool
            -> m LogReader
initSession build opt compOpts logging = do
    dflags0 <- getSessionDynFlags
    (dflags1,readLog) <- setupDynamicFlags dflags0
    _ <- setSessionDynFlags dflags1
    return readLog
  where
    cmdOpts = ghcOptions compOpts
    idirs   = includeDirs compOpts
    depPkgs = depPackages compOpts
    ls = lineSeparator opt
    setupDynamicFlags df0 = do
        df1 <- modifyFlagsWithOpts df0 cmdOpts
        let df2 = modifyFlags df1 idirs depPkgs (expandSplice opt) build
        df3 <- modifyFlagsWithOpts df2 $ ghcOpts opt
        liftIO $ setLogger logging df3 ls

----------------------------------------------------------------

-- | Initialize the 'DynFlags' relating to the compilation of a single
-- file or GHC session.
initializeFlags :: GhcMonad m => Options -> m ()
initializeFlags opt = do
    dflags0 <- getSessionDynFlags
    dflags1 <- modifyFlagsWithOpts dflags0 $ ghcOpts opt
    void $ setSessionDynFlags dflags1

----------------------------------------------------------------

-- FIXME removing Options
modifyFlags :: DynFlags -> [IncludeDir] -> [Package] -> Bool -> Build -> DynFlags
modifyFlags d0 idirs depPkgs splice build
  | splice    = setSplice d4
  | otherwise = d4
  where
    d1 = d0 { importPaths = idirs }
    d2 = setFastOrNot d1 Fast
    d3 = Gap.addDevPkgs d2 depPkgs
    d4 | build == CabalPkg = Gap.setCabalPkg d3
       | otherwise         = d3

setSplice :: DynFlags -> DynFlags
setSplice dflag = dopt_set dflag Opt_D_dump_splices

----------------------------------------------------------------

setFastOrNot :: DynFlags -> CheckSpeed -> DynFlags
setFastOrNot dflags Slow = dflags {
    ghcLink   = LinkInMemory
  , hscTarget = HscInterpreted
  }
setFastOrNot dflags Fast = dflags {
    ghcLink   = NoLink
  , hscTarget = HscNothing
  }

setSlowDynFlags :: GhcMonad m => m ()
setSlowDynFlags = (flip setFastOrNot Slow <$> getSessionDynFlags)
                  >>= void . setSessionDynFlags

-- | To check TH, a session module graph is necessary.
-- "load" sets a session module graph using "depanal".
-- But we have to set "-fno-code" to DynFlags before "load".
-- So, this is necessary redundancy.
checkSlowAndSet :: GhcMonad m => m ()
checkSlowAndSet = do
    fast <- canCheckFast <$> depanal [] False
    unless fast setSlowDynFlags

----------------------------------------------------------------

modifyFlagsWithOpts :: GhcMonad m => DynFlags -> [GHCOption] -> m DynFlags
modifyFlagsWithOpts dflags cmdOpts =
    tfst <$> parseDynamicFlags dflags (map noLoc cmdOpts)
  where
    tfst (a,_,_) = a

----------------------------------------------------------------

-- | Set the files that GHC will load / compile.
setTargetFiles :: (GhcMonad m) => [FilePath] -> m ()
setTargetFiles [] = error "ghc-mod: setTargetFiles: No target files given"
setTargetFiles files = do
    targets <- forM files $ \file -> guessTarget file Nothing
    setTargets targets

----------------------------------------------------------------

-- | Return the 'DynFlags' currently in use in the GHC session.
getDynamicFlags :: IO DynFlags
getDynamicFlags = runGhc (Just libdir) getSessionDynFlags

-- | Checking if Template Haskell or quasi quotes are used.
--   If not, the process can be faster.
canCheckFast :: ModuleGraph -> Bool
canCheckFast = not . any (hasTHorQQ . ms_hspp_opts)
  where
    hasTHorQQ :: DynFlags -> Bool
    hasTHorQQ dflags = any (`xopt` dflags) [Opt_TemplateHaskell, Opt_QuasiQuotes]
