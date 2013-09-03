module LangSpec where

import Control.Applicative
import Language.Haskell.GhcMod
import Test.Hspec

spec :: Spec
spec = do
    describe "listLanguages" $ do
        it "lists up language extensions" $ do
            exts <- lines <$> listLanguages defaultOptions
            exts `shouldContain` ["OverloadedStrings"]
