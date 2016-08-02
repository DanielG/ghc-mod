module CustomPackageDbSpec where

import Language.Haskell.GhcMod.CabalHelper
import Language.Haskell.GhcMod.CustomPackageDb
import Language.Haskell.GhcMod.Error
import System.Process
import Test.Hspec
import Prelude

import Dir
import TestUtils

spec :: Spec
spec = do
    describe "getCustomPkgDbStack" $ do
        it "works" $ do
            let tdir = "test/data/custom-cradle"
            Just stack <- runD' tdir $ getCustomPkgDbStack
            stack `shouldBe` [ GlobalDb
                             , PackageDb "package-db-a"
                             , PackageDb "package-db-b"
                             , PackageDb "package-db-c"
                             ]

    describe "getPackageDbStack'" $ do
        it "fixes out of sync custom pkg-db stack" $ do
            withDirectory_ "test/data/custom-cradle" $ do
                _ <- system "cabal configure"
                (s, s') <- runD $ do
                    Just stack <- getCustomPkgDbStack
                    withCabal $ do
                        stack' <- getCabalPackageDbStack
                        return (stack, stack')
                s' `shouldBe` s
