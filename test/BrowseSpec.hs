module BrowseSpec where

import Control.Applicative
import Language.Haskell.GhcMod
import Test.Hspec

spec :: Spec
spec = do
    describe "browseModule" $ do
        it "lists up symbols in the module" $ do
            syms <- lines <$> browseModule defaultOptions "Data.Map"
            syms `shouldContain` ["differenceWithKey"]

    describe "browseModule -d" $ do
        it "lists up symbols with type info in the module" $ do
            syms <- lines <$> browseModule defaultOptions { detailed = True } "Data.Either"
            syms `shouldContain` ["either :: (a -> c) -> (b -> c) -> Either a b -> c"]

        it "lists up data constructors with type info in the module" $ do
            syms <- lines <$> browseModule defaultOptions { detailed = True} "Data.Either"
            syms `shouldContain` ["Left :: a -> Either a b"]
