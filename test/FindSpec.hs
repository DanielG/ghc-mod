{-# LANGUAGE OverloadedStrings #-}
module FindSpec where

import Language.Haskell.GhcMod.Find
import Test.Hspec
import TestUtils

import Dir

spec :: Spec
spec = do
    describe "db <- loadSymbolDb" $ do
        it "lookupSymbol' db \"head\"  contains at least `Data.List'" $ do
            db <- runD $ loadSymbolDb
            lookupSym "head" db `shouldContain` [ModuleString "Data.List"]

        it "lookupSymbol' db \"untag\"  contains at least `Data.Tagged'" $ bracketTagged $ do
            db <- runD $ loadSymbolDb
            lookupSym "untag" db `shouldContain` [ModuleString "Data.Tagged"]

        it "lookupSymbol' db \"untag\" does not contain `Data.Tagged' if not installed" $ do
          withDirectory_ "test/data/options-cradle" $ do
            db <- runD $ loadSymbolDb
            lookupSym "untag" db `shouldNotContain` [ModuleString "Data.Tagged"]
