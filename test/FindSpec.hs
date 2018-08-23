{-# LANGUAGE OverloadedStrings #-}
module FindSpec where

import GhcMod.Exe.Find
import Test.Hspec
import TestUtils
import Dir

spec :: Spec
spec = do
    describe "db <- loadSymbolDb" $ do
        it "lookupSymbol' db \"head\"  contains at least `Data.List'" $ do
            withDirectory_ "test/data/ghc-mod-check" $ do
                db <- runD $ loadSymbolDb
                lookupSym "head" db `shouldContain` [ModuleString "Data.List"]
