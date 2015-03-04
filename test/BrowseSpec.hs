module BrowseSpec where

import Control.Applicative
import Language.Haskell.GhcMod
import Test.Hspec

import TestUtils
import Dir

spec :: Spec
spec = do
    describe "browse Data.Map" $ do
        it "contains at least `differenceWithKey'" $ do
            syms <- runD $ lines <$> browse "Data.Map"
            syms `shouldContain` ["differenceWithKey"]

    describe "browse -d Data.Either" $ do
        it "contains functions (e.g. `either') including their type signature" $ do
            syms <- run defaultOptions { detailed = True }
                    $ lines <$> browse "Data.Either"
            syms `shouldContain` ["either :: (a -> c) -> (b -> c) -> Either a b -> c"]

        it "contains type constructors (e.g. `Left') including their type signature" $ do
            syms <- run defaultOptions { detailed = True}
                    $ lines <$> browse "Data.Either"
            syms `shouldContain` ["Left :: a -> Either a b"]

    describe "`browse' in a project directory" $ do
        it "can list symbols defined in a a local module" $ do
            withDirectory_ "test/data/ghc-mod-check/lib" $ do
                syms <- runD $ lines <$> browse "Data.Foo"
                syms `shouldContain` ["foo"]
                syms `shouldContain` ["fibonacci"]
