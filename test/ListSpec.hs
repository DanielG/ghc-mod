module ListSpec where

import Control.Applicative
import Language.Haskell.GhcMod
import Test.Hspec
import TestUtils

spec :: Spec
spec = do
    describe "modules" $ do
        it "contains at least `Data.Map'" $ do
            modules <- runD $ lines <$> modules
            modules `shouldContain` ["Data.Map"]
