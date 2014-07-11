module BrowseSpec where

import Control.Applicative
import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Cradle
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
            cradle <- findCradle
            syms <- run defaultOptions { detailed = True}
                    $ lines <$> browse "Data.Either"
            syms `shouldContain` ["Left :: a -> Either a b"]

    describe "`browse' in a project directory" $ do
        it "lists symbols defined in a a local module (e.g. `Baz.baz)" $ do
            withDirectory_ "test/data" $ do
                syms <- runID $ lines <$> browse "Baz"
                syms `shouldContain` ["baz"]
