module FindSpec where

import Language.Haskell.GhcMod.Find
import Control.Monad
import Test.Hspec
import TestUtils

spec :: Spec
spec = do
    describe "db <- loadSymbolDb" $ do
        it "lookupSymbol' db \"head\"  contains at least `Data.List'" $ do
            db <- runD $ loadSymbolDb =<< (cradleTempDir `liftM` cradle)
            lookupSym "head" db `shouldContain` [ModuleString "Data.List"]
