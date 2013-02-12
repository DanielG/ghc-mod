module BrowseSpec where

import Control.Applicative
import Test.Hspec
import Browse
import Expectation
import Types

spec :: Spec
spec = do
    describe "browseModule" $ do
        it "lists up symbols in the module" $ do
            syms <- lines <$> browseModule defaultOptions "Data.Map"
            syms `shouldContain` "differenceWithKey"
