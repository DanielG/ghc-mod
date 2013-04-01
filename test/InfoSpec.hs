module InfoSpec where

import CabalApi
import Cradle
import Data.List (isPrefixOf)
import Expectation
import Info
import Test.Hspec
import Types
import System.Process
import System.Exit

spec :: Spec
spec = do
    describe "typeExpr" $ do
        it "shows types of the expression and its outers" $ do
            withDirectory_ "test/data/ghc-mod-check" $ do
                (strVer,_) <- getGHCVersion
                cradle <- findCradle Nothing strVer
                res <- typeExpr defaultOptions cradle "Data.Foo" 9 5 "Data/Foo.hs"
                res `shouldBe` "9 5 11 40 \"Int -> a -> a -> a\"\n7 1 11 40 \"Int -> Integer\"\n"

        it "works with a module using TemplateHaskell" $ do
            withDirectory_ "test/data" $ do
                cradle <- getGHCVersion >>= findCradle Nothing . fst
                res <- typeExpr defaultOptions cradle "Bar" 5 1 "Bar.hs"
                res `shouldBe` unlines ["5 1 5 20 \"[Char]\""]

        it "works with a module that imports another module using TemplateHaskell" $ do
            withDirectory_ "test/data" $ do
                cradle <- getGHCVersion >>= findCradle Nothing . fst
                res <- typeExpr defaultOptions cradle "Main" 3 8 "Main.hs"
                res `shouldBe` unlines ["3 8 3 16 \"String -> IO ()\"", "3 8 3 20 \"IO ()\"", "3 1 3 20 \"IO ()\""]

    describe "infoExpr" $ do
        it "works for non-export functions" $ do
            withDirectory_ "test/data" $ do
                cradle <- getGHCVersion >>= findCradle Nothing . fst
                res <- infoExpr defaultOptions cradle "Info" "fib" "Info.hs"
                res `shouldSatisfy` ("fib :: Int -> Int" `isPrefixOf`)

        it "works with a module using TemplateHaskell" $ do
            withDirectory_ "test/data" $ do
                cradle <- getGHCVersion >>= findCradle Nothing . fst
                res <- infoExpr defaultOptions cradle "Bar" "foo" "Bar.hs"
                res `shouldSatisfy` ("foo :: ExpQ" `isPrefixOf`)

        it "works with a module that imports another module using TemplateHaskell" $ do
            withDirectory_ "test/data" $ do
                cradle <- getGHCVersion >>= findCradle Nothing . fst
                res <- infoExpr defaultOptions cradle "Main" "bar" "Main.hs"
                res `shouldSatisfy` ("bar :: [Char]" `isPrefixOf`)

        it "doesn't fail on unicode output" $ do
            code <- rawSystem "dist/build/ghc-mod/ghc-mod" ["info", "test/data/Unicode.hs", "Unicode", "unicode"]
            code `shouldSatisfy` (== ExitSuccess)
