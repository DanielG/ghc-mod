module CaseSplitSpec where

import Language.Haskell.GhcMod
import Test.Hspec
import TestUtils
import Dir

main :: IO ()
main = do
  hspec spec

spec :: Spec
spec = do
    describe "case split" $ do
        it "does not blow up on HsWithBndrs panic" $ do
            withDirectory_ "test/data/case-split" $ do
                res <- runD $ splits "Vect.hs" 24 10
                res `shouldBe` "24 1 24 30"++
                        " \"mlAppend Nil y = _mlAppend_body\NUL"++
                           "mlAppend (Cons x1 x2) y = _mlAppend_body\"\n"

        it "works with case expressions" $ do
            withDirectory_ "test/data/case-split" $ do
                res <- runD $ splits "Vect.hs" 28 20
                res `shouldBe` "28 19 28 39"++
                        " \"Nil -> _mlAppend_body\NUL"++
                        "                  (Cons x'1 x'2) -> _mlAppend_body\"\n"

        it "works with where clauses" $ do
            withDirectory_ "test/data/case-split" $ do
                res <- runD $ splits "Vect.hs" 34 17
                res `shouldBe` "34 5 34 43"++
                        " \"mlReverse' Nil accum = _mlReverse_body\NUL"++
                        "    mlReverse' (Cons xs'1 xs'2) accum = _mlReverse_body\"\n"

        it "works with let bindings" $ do
            withDirectory_ "test/data/case-split" $ do
                res <- runD $ splits "Vect.hs" 38 33
                res `shouldBe` "38 21 38 59"++
                        " \"mlReverse' Nil accum = _mlReverse_body\NUL"++
                        "                    mlReverse' (Cons xs'1 xs'2) accum = _mlReverse_body\"\n"
