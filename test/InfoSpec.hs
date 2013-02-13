module InfoSpec where

import Test.Hspec
import Expectation
import Info
import Types

spec :: Spec
spec = do
    describe "typeExpr" $ do
        it "shows types of the expression and its outers" $ do
            withDirectory "test/data/ghc-mod-check" $ do
                res <- typeExpr defaultOptions "Data.Foo" 9 5 "Data/Foo.hs"
                res `shouldBe` "9 5 11 40 \"Int -> a -> a -> a\"\n7 1 11 40 \"Int -> Integer\"\n"
