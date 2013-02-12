module LintSpec where

import Test.Hspec
import Lint
import Types

spec :: Spec
spec = do
    describe "lintSyntax" $ do
        it "check syntax with HList" $ do
            res <- lintSyntax defaultOptions "test/data/hlint.hs"
            res `shouldBe` "test/data/hlint.hs:4:8: Error: Redundant do\NULFound:\NUL  do putStrLn \"Hello, world!\"\NULWhy not:\NUL  putStrLn \"Hello, world!\"\n"

