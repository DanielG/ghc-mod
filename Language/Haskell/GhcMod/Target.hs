module Language.Haskell.GhcMod.Target (
    setTargetFiles
  ) where

import Control.Applicative ((<$>))
import Control.Monad (forM, void, (>=>))
import DynFlags (ExtensionFlag(..), xopt)
import GHC (DynFlags(..), LoadHowMuch(..))
import qualified GHC as G
import Language.Haskell.GhcMod.DynFlags
import Language.Haskell.GhcMod.Monad

-- | Set the files as targets and load them.
setTargetFiles :: IOish m => [FilePath] -> GhcModT m ()
setTargetFiles files = do
    targets <- forM files $ \file -> G.guessTarget file Nothing
    G.setTargets targets
    mode <- getCompilerMode
    if mode == Intelligent then
        loadTargets Intelligent
      else do
        mdls <- G.depanal [] False
        let fallback = needsFallback mdls
        if fallback then do
            resetTargets targets
            setIntelligent
            loadTargets Intelligent
          else
            loadTargets Simple
  where
    loadTargets Simple = do
        -- Reporting error A and error B
        void $ G.load LoadAllTargets
        mss <- filter (\x -> G.ms_hspp_file x `elem` files) <$> G.getModuleGraph
        -- Reporting error B and error C
        mapM_ (G.parseModule >=> G.typecheckModule >=> G.desugarModule) mss
        -- Error B duplicates. But we cannot ignore both error reportings,
        -- sigh. So, the logger makes log messages unique by itself.
    loadTargets Intelligent = do
        df <- G.getSessionDynFlags
        void $ G.setSessionDynFlags (setModeIntelligent df)
        void $ G.load LoadAllTargets
    resetTargets targets = do
        G.setTargets []
        void $ G.load LoadAllTargets
        G.setTargets targets
    setIntelligent = do
        newdf <- setModeIntelligent <$> G.getSessionDynFlags
        void $ G.setSessionDynFlags newdf
        setCompilerMode Intelligent

needsFallback :: G.ModuleGraph -> Bool
needsFallback = any (hasTHorQQ . G.ms_hspp_opts)
  where
    hasTHorQQ :: DynFlags -> Bool
    hasTHorQQ dflags = any (`xopt` dflags) [Opt_TemplateHaskell, Opt_QuasiQuotes]
