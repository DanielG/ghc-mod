module TestUtils (
    run
  , runD
  , runI
  , runID
  , runIsolatedGhcMod
  , isolateCradle
  , module Language.Haskell.GhcMod.Monad
  , module Language.Haskell.GhcMod.Types
  ) where

import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Types

isolateCradle :: IOish m => GhcModT m a -> GhcModT m a
isolateCradle action =
    local modifyEnv  $ action
 where
    modifyEnv e = e { gmCradle = (gmCradle e) { cradlePkgDbStack = [GlobalDb] } }

runIsolatedGhcMod :: Options -> GhcModT IO a -> IO a
runIsolatedGhcMod opt action = runGhcModT opt $ isolateCradle action

-- | Run GhcMod in isolated cradle with default options
runID = runIsolatedGhcMod defaultOptions

-- | Run GhcMod in isolated cradle
runI = runIsolatedGhcMod

-- | Run GhcMod
run :: Options -> GhcModT IO a -> IO a
run = runGhcModT

-- | Run GhcMod with default options
runD :: GhcModT IO a -> IO a
runD = runGhcModT defaultOptions
