module ListSpec where

import Control.Applicative
import Language.Haskell.GhcMod
import Test.Hspec

spec :: Spec
spec = do
    describe "listModules" $ do
        it "lists up module names" $ do
            cradle <- findCradle
            modules <- lines <$> listModules defaultOptions cradle
            modules `shouldContain` ["Data.Map"]
