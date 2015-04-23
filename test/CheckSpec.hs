{-# LANGUAGE CPP #-}
module CheckSpec where

import Language.Haskell.GhcMod

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
