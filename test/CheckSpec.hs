module CheckSpec where

import Test.Hspec
import Check
import Expectation
import Types

spec :: Spec
spec = do
    describe "checkSyntax" $ do
        it "can check even if an executable depends on its library" $ do
            withDirectory "test/data/ghc-mod-check" $ do
                res <- checkSyntax defaultOptions "main.hs"
                res `shouldBe` "main.hs:5:1:Warning: Top-level binding with no type signature: main :: IO ()\NUL\n"
