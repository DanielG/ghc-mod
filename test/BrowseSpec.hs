module BrowseSpec where

import Control.Applicative
import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Cradle
import Test.Hspec

import TestUtils
import Dir

spec :: Spec
spec = do
    describe "browse" $ do
        it "lists up symbols in the module" $ do
            syms <- runD $ lines <$> browse "Data.Map"
            syms `shouldContain` ["differenceWithKey"]

    describe "browse -d" $ do
        it "lists up symbols with type info in the module" $ do
            syms <- run defaultOptions { detailed = True }
                    $ lines <$> browse "Data.Either"
            syms `shouldContain` ["either :: (a -> c) -> (b -> c) -> Either a b -> c"]

        it "lists up data constructors with type info in the module" $ do
            cradle <- findCradle
            syms <- run defaultOptions { detailed = True}
                    $ lines <$> browse "Data.Either"
            syms `shouldContain` ["Left :: a -> Either a b"]

    describe "browse local" $ do
        it "lists symbols in a local module" $ do
            withDirectory_ "test/data" $ do
                syms <- runID $ lines <$> browse "Baz"
                syms `shouldContain` ["baz"]
