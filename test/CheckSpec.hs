module CheckSpec where

import Check
import Cradle
import Expectation
import Test.Hspec
import Types

spec :: Spec
spec = do
    describe "checkSyntax" $ do
        it "can check even if an executable depends on its library" $ do
            withDirectory "test/data/ghc-mod-check" $ do
                cradle <- findCradle Nothing
                res <- checkSyntax defaultOptions cradle "main.hs"
                res `shouldBe` "main.hs:5:1:Warning: Top-level binding with no type signature: main :: IO ()\NUL\n"
