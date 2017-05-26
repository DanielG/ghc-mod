module ListSpec where

import Control.Applicative
import GhcMod
import Test.Hspec
import TestUtils
import Prelude

spec :: Spec
spec = do
    describe "modules" $ do
        it "contains at least `Data.Map'" $ do
            mdls <- runD $ lines <$> modules False
            mdls `shouldContain` ["Data.Map"]
