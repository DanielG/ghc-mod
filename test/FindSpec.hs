module FindSpec where

import Language.Haskell.GhcMod.Find
import Test.Hspec

spec :: Spec
spec = do
    describe "db <- loadSymbolDb" $ do
        it "lookupSymbol' db \"head\"  contains at least `Data.List'" $ do
            db <- loadSymbolDb
            lookupSym "head" db `shouldContain` ["Data.List"]
