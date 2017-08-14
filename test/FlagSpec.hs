module FlagSpec where

import Control.Applicative
import GhcMod
import Test.Hspec
import TestUtils
import Prelude

spec :: Spec
spec = do
    describe "flags" $ do
        it "contains at least `-fprint-explicit-foralls" $ do
            f <- runD $ lines <$> flags
            f `shouldContain` ["-fprint-explicit-foralls"]
