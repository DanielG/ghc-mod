module BrowseSpec where

import Control.Applicative
import GhcMod
import Test.Hspec
import Prelude

import TestUtils
import Dir

spec :: Spec
spec = do
    describe "browse Data.Map" $ do
        it "contains at least `differenceWithKey'" $ do
            syms <- runD $ lines <$> browse defaultBrowseOpts "Data.Map"
            syms `shouldContain` ["differenceWithKey"]

    describe "browse -d Data.Either" $ do
        it "contains functions (e.g. `either') including their type signature" $ do
            syms <- runD
                    $ lines <$> browse defaultBrowseOpts{ optBrowseDetailed = True } "Data.Either"
            syms `shouldContain` ["either :: (a -> c) -> (b -> c) -> Either a b -> c"]

        it "contains type constructors (e.g. `Left') including their type signature" $ do
            syms <- runD
                    $ lines <$> browse defaultBrowseOpts{ optBrowseDetailed = True } "Data.Either"
            syms `shouldContain` ["Left :: a -> Either a b"]

    describe "`browse' in a project directory" $ do
        it "can list symbols defined in a a local module" $ do
            withDirectory_ "test/data/ghc-mod-check/" $ do
                syms <- runD $ lines <$> browse defaultBrowseOpts "Data.Foo"
                syms `shouldContain` ["foo"]
                syms `shouldContain` ["fibonacci"]
