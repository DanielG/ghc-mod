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
            mdls <- runD $ lines <$> modules False
            mdls `shouldContain` ["Data.Map"]

        it "contains at least `Data.Tagged'" $ bracketTagged $ do
            mdls <- runD $ lines <$> modules False
            mdls `shouldContain` ["Data.Tagged"]

        it "does not contain `Data.Tagged'" $ do
            withDirectory_ "test/data/options-cradle" $ do
                mdls <- runD $ lines <$> modules False
                mdls `shouldNotContain` ["Data.Tagged"]
