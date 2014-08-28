module FindSpec where

import Language.Haskell.GhcMod.Find
import Test.Hspec
import TestUtils

spec :: Spec
spec = do
    describe "db <- loadSymbolDb" $ do
        it "lookupSymbol' db \"head\"  contains at least `Data.List'" $ do
            db <- runD loadSymbolDb
            lookupSym "head" db `shouldContain` ["Data.List"]
