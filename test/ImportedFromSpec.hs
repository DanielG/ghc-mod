{-# LANGUAGE CPP #-}
module ImportedFromSpec where

import Control.Applicative
import Language.Haskell.GhcMod
import System.FilePath
import Test.Hspec
import TestUtils
import Prelude

import Language.Haskell.GhcMod.Utils

---------------------------------------------------
import Language.Haskell.GhcMod.ImportedFrom
import System.FilePath()
import Test.Hspec

import Control.Exception as E
import System.Directory
---------------------------------------------------

isRight :: forall a b. Either a b -> Bool
isRight = either (const False) (const True)

spec :: Spec
spec = do
    describe "checkImportedFrom" $ do
        it "can look up Maybe" $ do
            withDirectory_ "test/data/imported-from" $ do
                (res, _) <- runGmOutDef $ runGhcModT defaultOptions $ importedFrom "ImportedFrom01.hs" 11 11 (Expression "Maybe")
                res `shouldSatisfy` isRight

        it "can look up Just" $ do
            withDirectory_ "test/data/imported-from" $ do
                (res, _) <- runGmOutDef $ runGhcModT defaultOptions $ importedFrom "ImportedFrom01.hs" 12 7  (Expression "Just")
                res `shouldSatisfy` isRight

        it "can look up Just" $ do
            withDirectory_ "test/data/imported-from" $ do
                (res, _) <- runGmOutDef $ runGhcModT defaultOptions $ importedFrom "ImportedFrom01.hs" 16 10 (Expression "Just")
                res `shouldSatisfy` isRight

        it "can look up String" $ do
            withDirectory_ "test/data/imported-from" $ do
                (res, _) <- runGmOutDef $ runGhcModT defaultOptions $ importedFrom "ImportedFrom01.hs" 20 14 (Expression "String")
                res `shouldSatisfy` isRight

        it "can look up Int" $ do
            withDirectory_ "test/data/imported-from" $ do
                (res, _) <- runGmOutDef $ runGhcModT defaultOptions $ importedFrom "ImportedFrom01.hs" 22 23 (Expression "Int")
                res `shouldSatisfy` isRight

        it "can look up DL.length" $ do
            withDirectory_ "test/data/imported-from" $ do
                (res, _) <- runGmOutDef $ runGhcModT defaultOptions $ importedFrom "ImportedFrom01.hs" 23 5  (Expression "DL.length")
                res `shouldSatisfy` isRight

        it "can look up print" $ do
            withDirectory_ "test/data/imported-from" $ do
                (res, _) <- runGmOutDef $ runGhcModT defaultOptions $ importedFrom "ImportedFrom01.hs" 25 8  (Expression "print")
                res `shouldSatisfy` isRight

        it "can look up DM.fromList" $ do
            withDirectory_ "test/data/imported-from" $ do
                (res, _) <- runGmOutDef $ runGhcModT defaultOptions $ importedFrom "ImportedFrom01.hs" 27 5  (Expression "DM.fromList")
                res `shouldSatisfy` isRight

        it "can look up Safe.headMay" $ do
            withDirectory_ "test/data/imported-from" $ do
                (res, _) <- runGmOutDef $ runGhcModT defaultOptions $ importedFrom "ImportedFrom01.hs" 29 6  (Expression "Safe.headMay")
                res `shouldSatisfy` isRight

        it "can look up Foo.Bar.length" $ do
            withDirectory_ "test/data/imported-from" $ do
                (res, _) <- runGmOutDef $ runGhcModT defaultOptions $ importedFrom "ImportedFrom01.hs" 34 17 (Expression "Foo.Bar.length")
                res `shouldSatisfy` isRight

        it "can look up map" $ do
            withDirectory_ "test/data/imported-from" $ do
                (res, _) <- runGmOutDef $ runGhcModT defaultOptions $ importedFrom "ImportedFrom02.hs" 14 5  (Expression "map")
                res `shouldSatisfy` isRight

        it "can look up head" $ do
            withDirectory_ "test/data/imported-from" $ do
                (res, _) <- runGmOutDef $ runGhcModT defaultOptions $ importedFrom "ImportedFrom02.hs" 16 5  (Expression "head")
                res `shouldSatisfy` isRight

        it "can look up when" $ do
            withDirectory_ "test/data/imported-from" $ do
                (res, _) <- runGmOutDef $ runGhcModT defaultOptions $ importedFrom "ImportedFrom03.hs"   15 5  (Expression "when")
                res `shouldSatisfy` isRight
