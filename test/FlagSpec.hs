module FlagSpec where

import Control.Applicative
import Language.Haskell.GhcMod
import Test.Hspec
import TestUtils

spec :: Spec
spec = do
    describe "flags" $ do
        it "contains at least `-fno-warn-orphans'" $ do
            f <- runD $ lines <$> flags
            f `shouldContain` ["-fno-warn-orphans"]
