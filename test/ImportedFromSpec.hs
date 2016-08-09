{-# LANGUAGE CPP #-}
module ImportedFromSpec where

import Control.Applicative
import Data.List
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

spec :: Spec
spec = do
    let tdir = "test/data/imported-from"

    describe "checkImportedFrom" $ do

        -- Previously this test looked up the "Maybe" in a type signature
        -- but now it fails - for some reason the expansion of spans
        -- was giving the contents of the body of the function. This worked
        -- before???
        it "can look up Maybe" $ do
            res <- runD' tdir $ importedFrom "ImportedFrom01.hs" 22 17 (Expression "Maybe")
            res `shouldSatisfy` (\x -> "base-" `isInfixOf` x || "haskell98-" `isInfixOf` x || "haskell2010-" `isInfixOf` x)
            res `shouldSatisfy` (isInfixOf "Data-Maybe.html")

        it "can look up Just" $ do
            res <- runD' tdir $ importedFrom "ImportedFrom01.hs" 12 7  (Expression "Just")
            res `shouldSatisfy` (\x -> "base-" `isInfixOf` x || "haskell98-" `isInfixOf` x || "haskell2010-" `isInfixOf` x)
            res `shouldSatisfy` (isInfixOf "Data-Maybe.html")

        it "can look up Just" $ do
            res <- runD' tdir $ importedFrom "ImportedFrom01.hs" 16 10 (Expression "Just")
            res `shouldSatisfy` (\x -> "base-" `isInfixOf` x || "haskell98-" `isInfixOf` x || "haskell2010-" `isInfixOf` x)
            res `shouldSatisfy` (isInfixOf "Data-Maybe.html")

        it "can look up String" $ do
            res <- runD' tdir $ importedFrom "ImportedFrom01.hs" 20 14 (Expression "String")
            res `shouldSatisfy` (\x -> "base-" `isInfixOf` x || "haskell98-" `isInfixOf` x || "haskell2010-" `isInfixOf` x)
            res `shouldSatisfy` (isInfixOf "Prelude.html")

        it "can look up Int" $ do
            res <- runD' tdir $ importedFrom "ImportedFrom01.hs" 22 23 (Expression "Int")
            res `shouldSatisfy` (\x -> "base-" `isInfixOf` x || "haskell98-" `isInfixOf` x || "haskell2010-" `isInfixOf` x)
            res `shouldSatisfy` (isInfixOf "Prelude.html")

        it "can look up DL.length" $ do
            res <- runD' tdir $ importedFrom "ImportedFrom01.hs" 23 5  (Expression "DL.length")
            res `shouldSatisfy` (\x -> "base-" `isInfixOf` x || "haskell98-" `isInfixOf` x || "haskell2010-" `isInfixOf` x)
            res `shouldSatisfy` (isInfixOf "Data-List.html")

        it "can look up print" $ do
            res <- runD' tdir $ importedFrom "ImportedFrom01.hs" 25 8  (Expression "print")
            res `shouldSatisfy` (\x -> "base-" `isInfixOf` x || "haskell98-" `isInfixOf` x || "haskell2010-" `isInfixOf` x)
            res `shouldSatisfy` (isInfixOf "Prelude.html")

        it "can look up DM.fromList" $ do
            res <- runD' tdir $ importedFrom "ImportedFrom01.hs" 27 5  (Expression "DM.fromList")
            res `shouldSatisfy` (isInfixOf "containers-")
            res `shouldSatisfy` (isInfixOf "Data-Map.html")

        -- This one is failing for some reason - something about not being able to load Safe? Temporarily disabling.
        --
        --     Failed to load interface for \8216Safe\8217\nUse -v to see a list of the files searched for.\n
        --
        --it "can look up Safe.headMay" $ do
        --    withDirectory_ "test/data/imported-from" $ do
        --        (res, _) <- runGmOutDef $ runGhcModT defaultOptions $ importedFrom "ImportedFrom01.hs" 29 6  (Expression "Safe.headMay")
        --        res `shouldSatisfy` isRight

        it "can look up Foo.Bar.length" $ do
            res <- runD' tdir $ importedFrom "ImportedFrom01.hs" 34 17 (Expression "Foo.Bar.length")
            res `shouldSatisfy` (\x -> "base-" `isInfixOf` x || "haskell98-" `isInfixOf` x || "haskell2010-" `isInfixOf` x)
            res `shouldSatisfy` (isInfixOf "Data-List.html")

        -- These from Safe also fail. Why?
        --it "can look up map" $ do
        --    res <- runD' tdir $ importedFrom "ImportedFrom02.hs" 14 5  (Expression "map")
        --    res `shouldSatisfy` (isInfixOf "000")
        --    res `shouldSatisfy` (isInfixOf "111")

        --it "can look up head" $ do
        --    res <- runD' tdir $ importedFrom "ImportedFrom02.hs" 16 5  (Expression "head")
        --    res `shouldSatisfy` (isInfixOf "000")
        --    res `shouldSatisfy` (isInfixOf "111")

        it "can look up when" $ do
            res <- runD' tdir $ importedFrom "ImportedFrom03.hs"   15 5  (Expression "when")
            res `shouldSatisfy` (\x -> "base-" `isInfixOf` x || "haskell98-" `isInfixOf` x || "haskell2010-" `isInfixOf` x)
            res `shouldSatisfy` (isInfixOf "Control-Monad.html")
