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
