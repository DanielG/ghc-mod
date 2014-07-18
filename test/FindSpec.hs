module FindSpec where

import Control.Applicative ((<$>))
import Data.List (isPrefixOf)
import Language.Haskell.GhcMod.Find
import Test.Hspec
import TestUtils

import qualified Data.Map

spec :: Spec
spec = do
    describe "db <- loadSymbolDb" $ do
        it "lookupSymbol' db \"head\"  contains at least `Data.List'" $ do
            db <- loadSymbolDb
            lookupSym "head" db `shouldContain` ["Data.List"]
