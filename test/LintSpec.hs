module LintSpec where

import GhcMod
import Test.Hspec
import TestUtils

spec :: Spec
spec = do
    describe "lint" $ do
        it "can detect a redundant import" $ do
            res <- runD $ lint lintOpts "test/data/hlint/hlint.hs"
            res `shouldBe` "test/data/hlint/hlint.hs:4:8: Warning: Redundant do\NULFound:\NUL  do putStrLn \"Hello, world!\"\NULWhy not:\NUL  putStrLn \"Hello, world!\"\n"

        context "when no suggestions are given" $ do
            it "doesn't output an empty line" $ do
                res <- runD $ lint lintOpts "test/data/ghc-mod-check/lib/Data/Foo.hs"
                res `shouldBe` ""

lintOpts :: LintOpts
lintOpts =
    defaultLintOpts { optLintHlintOpts = ["--ignore=Use module export list"] }
