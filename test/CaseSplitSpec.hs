{-# LANGUAGE CPP #-}
module CaseSplitSpec where

import Control.Applicative ((<$>))
import Data.List (isPrefixOf)
import Language.Haskell.GhcMod
#if __GLASGOW_HASKELL__ < 706
import System.Environment.Executable (getExecutablePath)
#else
import System.Environment (getExecutablePath)
#endif
import System.Exit
import System.FilePath
import System.Process
import Test.Hspec
import TestUtils
import Dir

spec :: Spec
spec = do
    describe "case split" $ do
        it "does not blow up on HsWithBndrs panic" $ do
            withDirectory_ "test/data/case-split" $ do
                res <- runD $ splits "Vect.hs" 24 10
                res `shouldBe` "9 5 11 40 \"Int -> a -> a -> a\"\n7 1 11 40 \"Int -> Integer\"\n"
