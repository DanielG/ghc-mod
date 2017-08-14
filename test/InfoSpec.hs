{-# LANGUAGE CPP #-}
module InfoSpec where

import Control.Applicative
import Data.List (isPrefixOf)
import GhcMod
#if __GLASGOW_HASKELL__ < 706
import System.Environment.Executable (getExecutablePath)
#else
import System.Environment (getExecutablePath)
#endif
import System.FilePath
import Test.Hspec
import TestUtils
import Prelude

spec :: Spec
spec = do
    describe "types" $ do
        it "shows types of the expression and its outers" $ do
            let tdir = "test/data/ghc-mod-check"
            res <- runD' tdir $ types False "lib/Data/Foo.hs" 9 5
#if __GLASGOW_HASKELL__ >= 800
            res `shouldBe` "9 5 11 40 \"Int -> t -> t -> t\"\n7 1 11 40 \"Int -> Integer\"\n"
#else
            res `shouldBe` "9 5 11 40 \"Int -> a -> a -> a\"\n7 1 11 40 \"Int -> Integer\"\n"
#endif


        it "shows types of the expression with constraints and its outers" $ do
            let tdir = "test/data/ghc-mod-check"
            res <- runD' tdir $ types True "lib/Data/Foo.hs" 9 5
#if __GLASGOW_HASKELL__ >= 800
            res `shouldBe` "9 5 11 40 \"Num t => Int -> t -> t -> t\"\n7 1 11 40 \"Int -> Integer\"\n"
#else
            res `shouldBe` "9 5 11 40 \"Num a => Int -> a -> a -> a\"\n7 1 11 40 \"Int -> Integer\"\n"
#endif

        it "works with a module using TemplateHaskell" $ do
            let tdir = "test/data/template-haskell"
            res <- runD' tdir $ types False "Bar.hs" 5 1
            res `shouldBe` unlines ["5 1 5 20 \"[Char]\""]

        it "works with a module that imports another module using TemplateHaskell" $ do
            let tdir = "test/data/template-haskell"
            res <- runD' tdir $ types False "ImportsTH.hs" 3 8
            res `shouldBe` unlines ["3 8 3 16 \"String -> IO ()\"", "3 8 3 20 \"IO ()\"", "3 1 3 20 \"IO ()\""]

    describe "info" $ do
        it "works for non exported functions" $ do
            let tdir = "test/data/non-exported"
            res <- runD' tdir $ info "Fib.hs" $ Expression "fib"
            res `shouldSatisfy` ("fib :: Int -> Int" `isPrefixOf`)

        it "works with a module using TemplateHaskell" $ do
            let tdir = "test/data/template-haskell"
            res <- runD' tdir $ info "Bar.hs" $ Expression "foo"
            res `shouldSatisfy` ("foo :: ExpQ" `isPrefixOf`)

        it "works with a module that imports another module using TemplateHaskell" $ do
            let tdir = "test/data/template-haskell"
            res <- runD' tdir $ info "ImportsTH.hs" $ Expression "bar"
            res `shouldSatisfy` ("bar :: [Char]" `isPrefixOf`)

getDistDir :: IO FilePath
getDistDir = takeDirectory . takeDirectory . takeDirectory <$> getExecutablePath
