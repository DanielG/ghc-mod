module CheckSpec where

import Data.List (isSuffixOf, isInfixOf, isPrefixOf)
import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Cradle
import System.FilePath
import Test.Hspec

import TestUtils
import Dir

spec :: Spec
spec = do
    describe "checkSyntax" $ do
        it "can check even if an executable depends on its library" $ do
            withDirectory_ "test/data/ghc-mod-check" $ do
                res <- runID $ checkSyntax ["main.hs"]
                res `shouldBe` "main.hs:5:1:Warning: Top-level binding with no type signature: main :: IO ()\n"

        it "can check even if a test module imports another test module located at different directory" $ do
            withDirectory_ "test/data/check-test-subdir" $ do
                res <- runID $ checkSyntax ["test/Bar/Baz.hs"]
                res `shouldSatisfy` (("test" </> "Foo.hs:3:1:Warning: Top-level binding with no type signature: foo :: [Char]\n") `isSuffixOf`)

        it "can detect mutually imported modules" $ do
            withDirectory_ "test/data" $ do
                res <- runID $ checkSyntax ["Mutual1.hs"]
                res `shouldSatisfy` ("Module imports form a cycle" `isInfixOf`)

        it "can check a module using QuasiQuotes" $ do
            withDirectory_ "test/data" $ do
                res <- runID $ checkSyntax ["Baz.hs"]
                res `shouldSatisfy` ("Baz.hs:5:1:Warning:" `isPrefixOf`)

        context "without errors" $ do
            it "doesn't output empty line" $ do
                withDirectory_ "test/data/ghc-mod-check/Data" $ do
                    res <- runID $ checkSyntax ["Foo.hs"]
                    res `shouldBe` ""
