module ListSpec where

import Control.Applicative
import Dir
import Language.Haskell.GhcMod
import Test.Hspec
import TestUtils
import Prelude

spec :: Spec
spec = do
    describe "modules" $ do
        it "contains at least `Data.Map'" $ do
            mdls <- runD $ lines <$> modules
            mdls `shouldContain` ["Data.Map"]

    describe "modules" $ do
        it "contains at least `Main'" $ do
            withDirectory_ "test/data/options-cradle" $ do
                mdls <- runD $ lines <$> modules
                mdls `shouldContain` ["Data.Map"]
