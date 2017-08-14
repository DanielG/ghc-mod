{-# LANGUAGE CPP #-}
module CheckSpec where

import GhcMod

import Data.List
import System.Process
import Test.Hspec

import TestUtils
import Dir

spec :: Spec
spec = do
    describe "checkSyntax" $ do
        it "works even if an executable depends on the library defined in the same cabal file" $ do
            withDirectory_ "test/data/ghc-mod-check" $ do
                res <- runD $ checkSyntax ["main.hs"]
                res `shouldBe` "main.hs:5:1:Warning: Top-level binding with no type signature: main :: IO ()\n"


        it "works even if a module imports another module from a different directory" $ do
            withDirectory_ "test/data/check-test-subdir" $ do
                _ <- system "cabal configure --enable-tests"
                res <- runD $ checkSyntax ["test/Bar/Baz.hs"]
                res `shouldSatisfy` (("test" </> "Foo.hs:3:1:Warning: Top-level binding with no type signature: foo :: [Char]\n") `isSuffixOf`)

        it "detects cyclic imports" $ do
            withDirectory_ "test/data/import-cycle" $ do
                res <- runD $ checkSyntax ["Mutual1.hs"]
                res `shouldSatisfy` ("Module imports form a cycle" `isInfixOf`)

        it "works with modules using QuasiQuotes" $ do
            withDirectory_ "test/data/quasi-quotes" $ do
                res <- runD $ checkSyntax ["QuasiQuotes.hs"]
                res `shouldSatisfy` ("QuasiQuotes.hs:6:1:Warning:" `isInfixOf`)

#if __GLASGOW_HASKELL__ >= 708
        it "works with modules using PatternSynonyms" $ do
            withDirectory_ "test/data/pattern-synonyms" $ do
                res <- runD $ checkSyntax ["B.hs"]
                res `shouldSatisfy` ("B.hs:6:9:Warning:" `isPrefixOf`)
#endif

        it "works with foreign exports" $ do
            withDirectory_ "test/data/foreign-export" $ do
                res <- runD $ checkSyntax ["ForeignExport.hs"]
                res `shouldBe` ""

        context "when no errors are found" $ do
            it "doesn't output an empty line" $ do
                withDirectory_ "test/data/ghc-mod-check/lib/Data" $ do
                    res <- runD $ checkSyntax ["Foo.hs"]
                    res `shouldBe` ""

#if __GLASGOW_HASKELL__ >= 708
-- See https://github.com/kazu-yamamoto/ghc-mod/issues/507
        it "emits warnings generated in GHC's desugar stage" $ do
            withDirectory_ "test/data/check-missing-warnings" $ do
                res <- runD $ checkSyntax ["DesugarWarnings.hs"]
                res `shouldSatisfy` ("DesugarWarnings.hs:4:9:Warning: Pattern match(es) are non-exhaustive\NULIn a case alternative: Patterns not matched:" `isPrefixOf`)
#endif

        it "works with cabal builtin preprocessors" $ do
            withDirectory_ "test/data/cabal-preprocessors" $ do
                _ <- system "cabal clean"
                _ <- system "cabal build"
                res <- runD $ checkSyntax ["Main.hs"]
                res `shouldBe` "Preprocessed.hsc:3:1:Warning: Top-level binding with no type signature: warning :: ()\n"

        it "Uses the right qualification style" $ do
            withDirectory_ "test/data/nice-qualification" $ do
                res <- runD $ checkSyntax ["NiceQualification.hs"]
#if __GLASGOW_HASKELL__ >= 800
                res `shouldBe` "NiceQualification.hs:4:8:\8226 Couldn't match expected type \8216IO ()\8217 with actual type \8216[Char]\8217\NUL\8226 In the expression: \"wrong type\"\NUL  In an equation for \8216main\8217: main = \"wrong type\"\n"
#elif __GLASGOW_HASKELL__ >= 708
                res `shouldBe` "NiceQualification.hs:4:8:Couldn't match expected type \8216IO ()\8217 with actual type \8216[Char]\8217\NULIn the expression: \"wrong type\"\NULIn an equation for \8216main\8217: main = \"wrong type\"\n"
#else
                res `shouldBe` "NiceQualification.hs:4:8:Couldn't match expected type `IO ()' with actual type `[Char]'\NULIn the expression: \"wrong type\"\NULIn an equation for `main': main = \"wrong type\"\n"
#endif
