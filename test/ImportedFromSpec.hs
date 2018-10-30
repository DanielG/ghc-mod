{-# LANGUAGE CPP #-}
module ImportedFromSpec where

import Data.List
import Language.Haskell.GhcMod
import Test.Hspec
import TestUtils
import Prelude

---------------------------------------------------
import Language.Haskell.GhcMod.ImportedFrom
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
            res <- runD' tdir $ importedFrom "ImportedFrom01.hs" 22 17 (Just (Expression "Maybe"))
            res `shouldSatisfy` (\x -> "base-" `isInfixOf` x || "haskell98-" `isInfixOf` x || "haskell2010-" `isInfixOf` x)
            res `shouldSatisfy` (isInfixOf "Data-Maybe.html")

        it "can look up Just" $ do
            res <- runD' tdir $ importedFrom "ImportedFrom01.hs" 12 7  (Just (Expression "Just"))
            res `shouldSatisfy` (\x -> "base-" `isInfixOf` x || "haskell98-" `isInfixOf` x || "haskell2010-" `isInfixOf` x)
            res `shouldSatisfy` (isInfixOf "Data-Maybe.html")

        it "can look up Just" $ do
            res <- runD' tdir $ importedFrom "ImportedFrom01.hs" 16 10 (Just (Expression "Just"))
            res `shouldSatisfy` (\x -> "base-" `isInfixOf` x || "haskell98-" `isInfixOf` x || "haskell2010-" `isInfixOf` x)
            res `shouldSatisfy` (isInfixOf "Data-Maybe.html")

        it "can look up String" $ do
            res <- runD' tdir $ importedFrom "ImportedFrom01.hs" 20 14 (Just (Expression "String"))
            res `shouldSatisfy` (\x -> "base-" `isInfixOf` x || "haskell98-" `isInfixOf` x || "haskell2010-" `isInfixOf` x)
            res `shouldSatisfy` (isInfixOf "Prelude.html")

        it "can look up Int" $ do
            res <- runD' tdir $ importedFrom "ImportedFrom01.hs" 22 23 (Just (Expression "Int"))
            res `shouldSatisfy` (\x -> "base-" `isInfixOf` x || "haskell98-" `isInfixOf` x || "haskell2010-" `isInfixOf` x)
            res `shouldSatisfy` (isInfixOf "Prelude.html")
            res `shouldSatisfy` (isPrefixOf "ghc-prim")

        it "can look up DL.length" $ do
            res <- runD' tdir $ importedFrom "ImportedFrom01.hs" 23 5  (Just (Expression "DL.length"))
            res `shouldSatisfy` (\x -> "base-" `isInfixOf` x || "haskell98-" `isInfixOf` x || "haskell2010-" `isInfixOf` x)
            res `shouldSatisfy` (isInfixOf "Data-List.html")

        it "can look up print" $ do
            res <- runD' tdir $ importedFrom "ImportedFrom01.hs" 25 8  (Just (Expression "print"))
            res `shouldSatisfy` (\x -> "base-" `isInfixOf` x || "haskell98-" `isInfixOf` x || "haskell2010-" `isInfixOf` x)
            res `shouldSatisfy` (isInfixOf "Prelude.html")

        it "can look up DM.fromList" $ do
            res <- runD' tdir $ importedFrom "ImportedFrom01.hs" 27 5  (Just (Expression "DM.fromList"))
            res `shouldSatisfy` (isInfixOf "containers-")
            res `shouldSatisfy` (isInfixOf "Data-Map.html")

        -- This one is failing for some reason - something about not being able to load Safe? Temporarily disabling.
        --
        --     Failed to load interface for \8216Safe\8217\nUse -v to see a list of the files searched for.\n
        --
        --it "can look up Safe.headMay" $ do
        --    withDirectory_ "test/data/imported-from" $ do
        --        (res, _) <- runGmOutDef $ runGhcModT defaultOptions $ importedFrom "ImportedFrom01.hs" 29 6  (Just (Expression "Safe.headMay"))
        --        res `shouldSatisfy` isRight

        it "can look up Foo.Bar.length" $ do
            res <- runD' tdir $ importedFrom "ImportedFrom01.hs" 34 17 (Just (Expression "Foo.Bar.length"))
            res `shouldSatisfy` (\x -> "base-" `isInfixOf` x || "haskell98-" `isInfixOf` x || "haskell2010-" `isInfixOf` x)
            res `shouldSatisfy` (isInfixOf "Data-List.html")

        -- These from Safe also fail. Why?
        --it "can look up map" $ do
        --    res <- runD' tdir $ importedFrom "ImportedFrom02.hs" 14 5  (Just (Expression "map"))
        --    res `shouldSatisfy` (isInfixOf "000")
        --    res `shouldSatisfy` (isInfixOf "111")

        --it "can look up head" $ do
        --    res <- runD' tdir $ importedFrom "ImportedFrom02.hs" 16 5  (Just (Expression "head"))
        --    res `shouldSatisfy` (isInfixOf "000")
        --    res `shouldSatisfy` (isInfixOf "111")

        it "can look up when" $ do
            res <- runD' tdir $ importedFrom "ImportedFrom03.hs"   15 5  (Just (Expression "when"))
            res `shouldSatisfy` (\x -> "base-" `isInfixOf` x || "haskell98-" `isInfixOf` x || "haskell2010-" `isInfixOf` x)
            res `shouldSatisfy` (isInfixOf "Control-Monad.html")
