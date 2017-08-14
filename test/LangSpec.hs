module LangSpec where

import Control.Applicative
import GhcMod
import Test.Hspec
import TestUtils
import Prelude

spec :: Spec
spec = do
    describe "languages" $ do
        it "contains at lest `OverloadedStrings'" $ do
            exts <- runD $ lines <$> languages
            exts `shouldContain` ["OverloadedStrings"]
