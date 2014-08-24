module TestUtils (
    run
  , runD
  , runD'
  , runI
  , runID
  , runIsolatedGhcMod
  , isolateCradle
  , shouldReturnError
  , module Language.Haskell.GhcMod.Monad
  , module Language.Haskell.GhcMod.Types
  ) where

import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Types

import Test.Hspec

isolateCradle :: IOish m => GhcModT m a -> GhcModT m a
isolateCradle action =
    local modifyEnv  $ action
 where
    modifyEnv e = e { gmCradle = (gmCradle e) { cradlePkgDbStack = [GlobalDb] } }

extract :: Show e => IO (Either e a, w) -> IO a
extract action = do
  (r,_) <- action
  case r of
    Right a ->  return a
    Left e -> error $ show e

runIsolatedGhcMod :: Options -> GhcModT IO a -> IO a
runIsolatedGhcMod opt action = do
  extract $ runGhcModT opt $ isolateCradle action

-- | Run GhcMod in isolated cradle with default options
runID :: GhcModT IO a -> IO a
runID = runIsolatedGhcMod defaultOptions

-- | Run GhcMod in isolated cradle
runI :: Options -> GhcModT IO a -> IO a
runI = runIsolatedGhcMod

-- | Run GhcMod
run :: Options -> GhcModT IO a -> IO a
run opt a = extract $ runGhcModT opt a

-- | Run GhcMod with default options
runD :: GhcModT IO a -> IO a
runD = extract . runGhcModT defaultOptions

runD' :: GhcModT IO a -> IO (Either GhcModError a, GhcModLog)
runD' = runGhcModT defaultOptions

shouldReturnError :: Show a
                  => IO (Either GhcModError a, GhcModLog)
                  -> Expectation
shouldReturnError action = do
  (a,_) <- action
  a `shouldSatisfy` isLeft
 where
   isLeft (Left _) = True
   isLeft _ = False
