module FlagSpec where

import Control.Applicative
import Language.Haskell.GhcMod
import Test.Hspec

spec :: Spec
spec = do
    describe "listFlags" $ do
        it "lists up GHC flags" $ do
            flags <- lines <$> listFlags defaultOptions
            flags `shouldContain` ["-fno-warn-orphans"]
