{-# LANGUAGE CPP #-}
module Language.Haskell.GhcMod.Target (
    setTargetFiles
  ) where
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

import Control.Applicative ((<$>))
import Control.Monad (forM, void, (>=>))
import DynFlags (ExtensionFlag(..), xopt)
import GHC (LoadHowMuch(..))
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
needsFallback = any $ \ms ->
                let df = G.ms_hspp_opts ms in
                   Opt_TemplateHaskell `xopt` df
                || Opt_QuasiQuotes     `xopt` df
#if __GLASGOW_HASKELL__ >= 708
                || (Opt_PatternSynonyms `xopt` df)
#endif
