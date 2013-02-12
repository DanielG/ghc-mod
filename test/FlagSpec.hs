module FlagSpec where

import Control.Applicative
import Test.Hspec
import Expectation
import Flag
import Types

spec :: Spec
spec = do
    describe "listFlags" $ do
        it "lists up GHC flags" $ do
            flags <- lines <$> listFlags defaultOptions
            flags `shouldContain` "-fno-warn-orphans"
