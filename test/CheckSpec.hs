module CheckSpec where

import CabalApi
import Check
import Cradle
import Data.List (isSuffixOf)
import Expectation
import Test.Hspec
import Types

spec :: Spec
spec = do
    describe "checkSyntax" $ do
        it "can check even if an executable depends on its library" $ do
            withDirectory_ "test/data/ghc-mod-check" $ do
                (strVer,_) <- getGHCVersion
                cradle <- findCradle Nothing strVer
                res <- checkSyntax defaultOptions cradle "main.hs"
                res `shouldBe` "main.hs:5:1:Warning: Top-level binding with no type signature: main :: IO ()\NUL\n"

        it "can check even if a test module imports another test module located at different directory" $ do
            withDirectory_ "test/data/check-test-subdir" $ do
                cradle <- getGHCVersion >>= findCradle Nothing . fst
                res <- checkSyntax defaultOptions cradle "test/Bar/Baz.hs"
                res `shouldSatisfy` ("test/Foo.hs:3:1:Warning: Top-level binding with no type signature: foo :: [Char]\NUL\n" `isSuffixOf`)
