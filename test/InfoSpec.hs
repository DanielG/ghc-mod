{-# LANGUAGE CPP #-}
module InfoSpec where

import Control.Applicative ((<$>))
import Data.List (isPrefixOf)
import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Cradle
#if __GLASGOW_HASKELL__ < 706
import System.Environment.Executable (getExecutablePath)
#else
import System.Environment (getExecutablePath)
#endif
import System.Exit
import System.FilePath
import System.Process
import Test.Hspec

import Dir

spec :: Spec
spec = do
    describe "typeExpr" $ do
        it "shows types of the expression and its outers" $ do
            withDirectory_ "test/data/ghc-mod-check" $ do
                cradle <- findCradleWithoutSandbox
                res <- typeExpr defaultOptions cradle "Data/Foo.hs" 9 5
                res `shouldBe` "9 5 11 40 \"Int -> a -> a -> a\"\n7 1 11 40 \"Int -> Integer\"\n"

        it "works with a module using TemplateHaskell" $ do
            withDirectory_ "test/data" $ do
                cradle <- findCradleWithoutSandbox
                res <- typeExpr defaultOptions cradle "Bar.hs" 5 1
                res `shouldBe` unlines ["5 1 5 20 \"[Char]\""]

        it "works with a module that imports another module using TemplateHaskell" $ do
            withDirectory_ "test/data" $ do
                cradle <- findCradleWithoutSandbox
                res <- typeExpr defaultOptions cradle "Main.hs" 3 8
                res `shouldBe` unlines ["3 8 3 16 \"String -> IO ()\"", "3 8 3 20 \"IO ()\"", "3 1 3 20 \"IO ()\""]

    describe "infoExpr" $ do
        it "works for non-export functions" $ do
            withDirectory_ "test/data" $ do
                cradle <- findCradleWithoutSandbox
                res <- infoExpr defaultOptions cradle "Info.hs" "fib"
                res `shouldSatisfy` ("fib :: Int -> Int" `isPrefixOf`)

        it "works with a module using TemplateHaskell" $ do
            withDirectory_ "test/data" $ do
                cradle <- findCradleWithoutSandbox
                res <- infoExpr defaultOptions cradle "Bar.hs" "foo"
                res `shouldSatisfy` ("foo :: ExpQ" `isPrefixOf`)

        it "works with a module that imports another module using TemplateHaskell" $ do
            withDirectory_ "test/data" $ do
                cradle <- findCradleWithoutSandbox
                res <- infoExpr defaultOptions cradle "Main.hs" "bar"
                res `shouldSatisfy` ("bar :: [Char]" `isPrefixOf`)

        it "doesn't fail on unicode output" $ do
            dir <- getDistDir
            code <- rawSystem (dir </> "build/ghc-mod/ghc-mod") ["info", "test/data/Unicode.hs", "Unicode", "unicode"]
            code `shouldSatisfy` (== ExitSuccess)

getDistDir :: IO FilePath
getDistDir = takeDirectory . takeDirectory . takeDirectory <$> getExecutablePath
