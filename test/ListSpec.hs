module ListSpec where

import Control.Applicative
import Language.Haskell.GhcMod
import Test.Hspec

spec :: Spec
spec = do
    describe "listModules" $ do
        it "lists up module names" $ do
            modules <- lines <$> listModules defaultOptions
            modules `shouldContain` ["Data.Map"]
