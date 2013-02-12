module ListSpec where

import Control.Applicative
import Test.Hspec
import Expectation
import List
import Types

spec :: Spec
spec = do
    describe "listModules" $ do
        it "lists up module names" $ do
            modules <- lines <$> listModules defaultOptions
            modules `shouldContain` "Data.Map"
