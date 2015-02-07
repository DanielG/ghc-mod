module TestUtils (
    run
  , runD
  , runD'
  , runI
--  , runID
  , runIsolatedGhcMod
  , isolateCradle
  , shouldReturnError
  , isPkgDbAt
  , isPkgConfDAt
  , module Language.Haskell.GhcMod.Monad
  , module Language.Haskell.GhcMod.Types
  ) where

import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Types

import Data.List.Split
import System.FilePath
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
--runID :: GhcModT IO a -> IO a
--runID = runIsolatedGhcMod defaultOptions

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

isPkgConfD :: FilePath -> Bool
isPkgConfD d = let
    (_dir, pkgconfd) = splitFileName d
    in case splitOn "-" pkgconfd of
         [_arch, _platform, _compiler, _compver, "packages.conf.d"] -> True
         _ -> False

isPkgConfDAt :: FilePath -> FilePath -> Bool
isPkgConfDAt d d' | d == takeDirectory d' && isPkgConfD d' = True
isPkgConfDAt _ _ = False

isPkgDbAt :: FilePath -> GhcPkgDb -> Bool
isPkgDbAt d (PackageDb dir) = isPkgConfDAt d dir
isPkgDbAt _ _ = False
