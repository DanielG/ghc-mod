module GhcPkgSpec where

import GhcMod.GhcPkg
import GhcMod.CabalHelper
import GhcMod.CustomPackageDb
import Test.Hspec
import System.Process (system)

import Dir
import TestUtils

spec :: Spec
spec = do
    describe "getPackageDbStack'" $ do
        it "fixes out of sync custom pkg-db stack" $ do
            withDirectory_ "test/data/custom-cradle" $ do
                _ <- system "cabal configure"
                (s, s') <- runD $ do
                    Just stack <- getCustomPkgDbStack
                    withCabal $ do
                        stack' <- getPackageDbStack
                        return (stack, stack')
                s' `shouldBe` s
