module LangSpec where

import Control.Applicative
import Test.Hspec
import Expectation
import Lang
import Types

spec :: Spec
spec = do
    describe "listLanguages" $ do
        it "lists up language extensions" $ do
            exts <- lines <$> listLanguages defaultOptions
            exts `shouldContain` "OverloadedStrings"
