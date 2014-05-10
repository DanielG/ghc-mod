module LintSpec where

import Language.Haskell.GhcMod
import Test.Hspec

spec :: Spec
spec = do
    describe "lintSyntax" $ do
        it "check syntax with HLint" $ do
            res <- lintSyntax defaultOptions "test/data/hlint.hs"
            res `shouldBe` "test/data/hlint.hs:4:8: Error: Redundant do\NULFound:\NUL  do putStrLn \"Hello, world!\"\NULWhy not:\NUL  putStrLn \"Hello, world!\"\n"

        context "without suggestions" $ do
            it "doesn't output empty line" $ do
                res <- lintSyntax defaultOptions "test/data/ghc-mod-check/Data/Foo.hs"
                res `shouldBe` ""
