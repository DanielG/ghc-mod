module Language.Haskell.GhcMod.DynFlags where

import qualified Language.Haskell.GhcMod.Gap as Gap
import Language.Haskell.GhcMod.Types

import Control.Applicative ((<$>))
import Control.Monad (forM, void, (>=>))
import GHC (DynFlags(..), GhcMode(..), GhcLink(..), HscTarget(..), LoadHowMuch(..))
import qualified GHC as G
import GhcMonad
import GHC.Paths (libdir)
import DynFlags (ExtensionFlag(..), xopt)

import System.IO.Unsafe (unsafePerformIO)

data Build = CabalPkg | SingleFile deriving Eq

setEmptyLogger :: DynFlags -> DynFlags
setEmptyLogger df = Gap.setLogAction df $ \_ _ _ _ _ -> return ()

-- Fast
-- Friendly to foreign export
-- Not friendly to Template Haskell
-- Uses small memory
setModeSimple :: DynFlags -> DynFlags
setModeSimple df = df {
    ghcMode   = CompManager
  , ghcLink   = NoLink
  , hscTarget = HscNothing
  }

-- Slow
-- Not friendly to foreign export
-- Friendly to Template Haskell
-- Uses large memory
setModeIntelligent :: DynFlags -> DynFlags
setModeIntelligent df = df {
    ghcMode   = CompManager
  , ghcLink   = LinkInMemory
  , hscTarget = HscInterpreted
  }

setIncludeDirs :: [IncludeDir] -> DynFlags -> DynFlags
setIncludeDirs idirs df = df { importPaths = idirs }

setBuildEnv :: Build -> DynFlags -> DynFlags
setBuildEnv build = setHideAllPackages build . setCabalPackage build

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

-- | Set the files as targets and load them.
setTargetFiles :: (GhcMonad m) => [FilePath] -> m ()
setTargetFiles files = do
    targets <- forM files $ \file -> G.guessTarget file Nothing
    G.setTargets targets
    xs <- G.depanal [] False
    -- FIXME, checking state
    loadTargets $ needsFallback xs
  where
    loadTargets False = do
        -- Reporting error A and error B
        void $ G.load LoadAllTargets
        mss <- filter (\x -> G.ms_hspp_file x `elem` files) <$> G.getModuleGraph
        -- Reporting error B and error C
        mapM_ (G.parseModule >=> G.typecheckModule >=> G.desugarModule) mss
        -- Error B duplicates. But we cannot ignore both error reportings,
        -- sigh. So, the logger makes log messages unique by itself.
    loadTargets True = do
        df <- G.getSessionDynFlags
        void $ G.setSessionDynFlags (setModeIntelligent df)
        void $ G.load LoadAllTargets

needsFallback :: G.ModuleGraph -> Bool
needsFallback = any (hasTHorQQ . G.ms_hspp_opts)
  where
    hasTHorQQ :: DynFlags -> Bool
    hasTHorQQ dflags = any (`xopt` dflags) [Opt_TemplateHaskell, Opt_QuasiQuotes]

----------------------------------------------------------------

-- | Return the 'DynFlags' currently in use in the GHC session.
getDynamicFlags :: IO DynFlags
getDynamicFlags = do
    G.runGhc (Just libdir) G.getSessionDynFlags

withDynFlags :: GhcMonad m
             => (DynFlags -> DynFlags)
             -> m a
             -> m a
withDynFlags setFlags body = G.gbracket setup teardown (\_ -> body)
  where
    setup = do
        dflags <- G.getSessionDynFlags
        void $ G.setSessionDynFlags (setFlags dflags)
        return dflags
    teardown = void . G.setSessionDynFlags

withCmdFlags :: GhcMonad m => [GHCOption] -> m a -> m a
withCmdFlags flags body = G.gbracket setup teardown (\_ -> body)
  where
    setup = do
        dflags <- G.getSessionDynFlags >>= addCmdOpts flags
        void $ G.setSessionDynFlags dflags
        return dflags
    teardown = void . G.setSessionDynFlags

----------------------------------------------------------------

-- | Set 'DynFlags' equivalent to "-w:".
setNoWaringFlags :: DynFlags -> DynFlags
setNoWaringFlags df = df { warningFlags = Gap.emptyWarnFlags}

-- | Set 'DynFlags' equivalent to "-Wall".
setAllWaringFlags :: DynFlags -> DynFlags
setAllWaringFlags df = df { warningFlags = allWarningFlags }

allWarningFlags :: Gap.WarnFlags
allWarningFlags = unsafePerformIO $ do
    G.runGhc (Just libdir) $ do
        df <- G.getSessionDynFlags
        df' <- addCmdOpts ["-Wall"] df
        return $ G.warningFlags df'

----------------------------------------------------------------
