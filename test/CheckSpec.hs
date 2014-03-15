module CheckSpec where

import Data.List (isSuffixOf, isInfixOf, isPrefixOf)
import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Cradle
import System.FilePath
import Test.Hspec

import Dir

spec :: Spec
spec = do
    describe "checkSyntax" $ do
        it "can check even if an executable depends on its library" $ do
            withDirectory_ "test/data/ghc-mod-check" $ do
                cradle <- findCradleWithoutSandbox
                res <- checkSyntax defaultOptions cradle ["main.hs"]
                res `shouldBe` "main.hs:5:1:Warning: Top-level binding with no type signature: main :: IO ()\NUL\n"

        it "can check even if a test module imports another test module located at different directory" $ do
            withDirectory_ "test/data/check-test-subdir" $ do
                cradle <- findCradleWithoutSandbox
                res <- checkSyntax defaultOptions cradle ["test/Bar/Baz.hs"]
                res `shouldSatisfy` (("test" </> "Foo.hs:3:1:Warning: Top-level binding with no type signature: foo :: [Char]\NUL\n") `isSuffixOf`)

        it "can detect mutually imported modules" $ do
            withDirectory_ "test/data" $ do
                cradle <- findCradleWithoutSandbox
                res <- checkSyntax defaultOptions cradle ["Mutual1.hs"]
                res `shouldSatisfy` ("Module imports form a cycle" `isInfixOf`)

        it "can check a module using QuasiQuotes" $ do
            withDirectory_ "test/data" $ do
                cradle <- findCradleWithoutSandbox
                res <- checkSyntax defaultOptions cradle ["Baz.hs"]
                res `shouldSatisfy` ("Baz.hs:5:1:Warning:" `isPrefixOf`)

        it "can check pattern matching warnings" $ do
            withDirectory_ "test/data" $ do
                cradle <- getGHCVersion >>= findCradle Nothing . fst
                res <- checkSyntax defaultOptions cradle ["Pattern.hs"]
                res `shouldSatisfy` ("Pattern.hs:3:1:Warning: Pattern match(es) are non-exhaustive" `isPrefixOf`)
