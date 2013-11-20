module BrowseSpec where

import Control.Applicative
import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Cradle
import Test.Hspec

import Dir

spec :: Spec
spec = do
    describe "browseModule" $ do
        it "lists up symbols in the module" $ do
            cradle <- findCradle
            syms <- lines <$> browseModule defaultOptions cradle "Data.Map"
            syms `shouldContain` ["differenceWithKey"]

    describe "browseModule -d" $ do
        it "lists up symbols with type info in the module" $ do
            cradle <- findCradle
            syms <- lines <$> browseModule defaultOptions { detailed = True } cradle "Data.Either"
            syms `shouldContain` ["either :: (a -> c) -> (b -> c) -> Either a b -> c"]

        it "lists up data constructors with type info in the module" $ do
            cradle <- findCradle
            syms <- lines <$> browseModule defaultOptions { detailed = True} cradle "Data.Either"
            syms `shouldContain` ["Left :: a -> Either a b"]

    describe "browseModule local" $ do
        it "lists symbols in a local module" $ do
            withDirectory_ "test/data" $ do
                cradle <- findCradleWithoutSandbox
                syms <- lines <$> browseModule defaultOptions cradle "Baz"
                syms `shouldContain` ["baz"]
