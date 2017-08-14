{-# LANGUAGE OverloadedStrings #-}
module FindSpec where

import GhcMod.Exe.Find
import Test.Hspec
import TestUtils

spec :: Spec
spec = do
    describe "db <- loadSymbolDb" $ do
        it "lookupSymbol' db \"head\"  contains at least `Data.List'" $ do
            db <- runD $ loadSymbolDb
            lookupSym "head" db `shouldContain` [ModuleString "Data.List"]
