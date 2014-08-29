{-# LANGUAGE CPP #-}

module Language.Haskell.GhcMod.DynFlags where

import Control.Applicative ((<$>))
import Control.Monad (void)
import GHC (DynFlags(..), GhcMode(..), GhcLink(..), HscTarget(..))
import qualified GHC as G
import GHC.Paths (libdir)
import GhcMonad
import qualified Language.Haskell.GhcMod.Gap as Gap
import Language.Haskell.GhcMod.Types
import System.IO.Unsafe (unsafePerformIO)

data Build = CabalPkg | SingleFile deriving Eq

setEmptyLogger :: DynFlags -> DynFlags
setEmptyLogger df = Gap.setLogAction df $ \_ _ _ _ _ -> return ()

-- * Fast
-- * Friendly to foreign export
-- * Not friendly to -XTemplateHaskell and -XPatternSynonyms
-- * Uses little memory
setModeSimple :: DynFlags -> DynFlags
setModeSimple df = df {
    ghcMode   = CompManager
  , ghcLink   = NoLink
  , hscTarget = HscNothing
  , optLevel  = 0
  }

-- * Slow
-- * Not friendly to foreign export
-- * Friendly to -XTemplateHaskell and -XPatternSynonyms
-- * Uses lots of memory
setModeIntelligent :: DynFlags -> DynFlags
setModeIntelligent df = df {
    ghcMode   = CompManager
  , ghcLink   = LinkInMemory
  , hscTarget = HscInterpreted
  , optLevel  = 0
  }

setIncludeDirs :: [IncludeDir] -> DynFlags -> DynFlags
setIncludeDirs idirs df = df { importPaths = idirs }

setBuildEnv :: Build -> DynFlags -> DynFlags
setBuildEnv build = setHideAllPackages build . setCabalPackage build

-- | With ghc-7.8 this option simply makes GHC print a message suggesting users
-- add hiddend packages to the build-depends field in their cabal file when the
-- user tries to import a module form a hidden package.
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

-- | Return the 'DynFlags' currently in use in the GHC session.
getDynamicFlags :: IO DynFlags
getDynamicFlags = G.runGhc (Just libdir) G.getSessionDynFlags

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
        dflags <- G.getSessionDynFlags
        void $ G.setSessionDynFlags =<< addCmdOpts flags dflags
        return dflags
    teardown = void . G.setSessionDynFlags

----------------------------------------------------------------

-- | Set 'DynFlags' equivalent to "-w:".
setNoWarningFlags :: DynFlags -> DynFlags
setNoWarningFlags df = df { warningFlags = Gap.emptyWarnFlags}

-- | Set 'DynFlags' equivalent to "-Wall".
setAllWarningFlags :: DynFlags -> DynFlags
setAllWarningFlags df = df { warningFlags = allWarningFlags }

allWarningFlags :: Gap.WarnFlags
allWarningFlags = unsafePerformIO $
    G.runGhc (Just libdir) $ do
        df <- G.getSessionDynFlags
        df' <- addCmdOpts ["-Wall"] df
        return $ G.warningFlags df'

----------------------------------------------------------------

-- | Set 'DynFlags' equivalent to "-fno-max-relevant-bindings".
setNoMaxRelevantBindings :: DynFlags -> DynFlags
#if __GLASGOW_HASKELL__ >= 708
setNoMaxRelevantBindings df = df { maxRelevantBinds = Nothing }
#else
setNoMaxRelevantBindings = id
#endif
