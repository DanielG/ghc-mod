{-# LANGUAGE CPP #-}
module CaseSplitSpec where

import GhcMod
import Test.Hspec
import TestUtils
import Dir

main :: IO ()
main = do
  hspec spec

spec :: Spec
spec = do
    describe "case split" $ do
#if __GLASGOW_HASKELL__ >= 708
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
#else
        it "does not blow up on HsWithBndrs panic" $ do
            withDirectory_ "test/data/case-split" $ do
                res <- runD $ splits "Vect706.hs" 24 10
                res `shouldBe` "24 1 24 25"++
                        " \"mlAppend Nil y = undefined\NUL"++
                           "mlAppend (Cons x1 x2) y = undefined\"\n"

        it "works with case expressions" $ do
            withDirectory_ "test/data/case-split" $ do
                res <- runD $ splits "Vect706.hs" 28 20
                res `shouldBe` "28 19 28 34"++
                        " \"Nil -> undefined\NUL"++
                        "                  (Cons x'1 x'2) -> undefined\"\n"

        it "works with where clauses" $ do
            withDirectory_ "test/data/case-split" $ do
                res <- runD $ splits "Vect706.hs" 34 17
                res `shouldBe` "34 5 34 37"++
                        " \"mlReverse' Nil accum = undefined\NUL"++
                        "    mlReverse' (Cons xs'1 xs'2) accum = undefined\"\n"

        it "works with let bindings" $ do
            withDirectory_ "test/data/case-split" $ do
                res <- runD $ splits "Vect706.hs" 38 33
                res `shouldBe` "38 21 38 53"++
                        " \"mlReverse' Nil accum = undefined\NUL"++
                        "                    mlReverse' (Cons xs'1 xs'2) accum = undefined\"\n"

#endif
        it "doesn't crash when source doesn't make sense" $
            withDirectory_ "test/data/case-split" $ do
                res <- runD $ splits "Crash.hs" 4 6
#if __GLASGOW_HASKELL__ < 710
                res `shouldBe` "4 1 4 19 \"test x = undefined\"\n"
#else
                res `shouldBe` ""
#endif
