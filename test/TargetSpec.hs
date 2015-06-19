{-# LANGUAGE OverloadedStrings #-}
module TargetSpec where

import Language.Haskell.GhcMod.Target
import Language.Haskell.GhcMod.Gap
import Test.Hspec

import TestUtils

import GHC
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath

spec :: Spec
spec = do
    describe "runLightGhc" $ do
        it "works at all" $ do
            withLightHscEnv [] $ \env ->
              runLightGhc env (return ()) `shouldReturn` ()

        it "has modules in scope" $ do
            withLightHscEnv [] $ \env ->
              runLightGhc env $ do
               dflags <- getSessionDynFlags
               let i = intersect (listVisibleModuleNames dflags)
                                 ["Control.Applicative", "Control.Arrow"
                                 ,"Control.Exception", "GHC.Exts", "GHC.Float"]
               liftIO $ i `shouldSatisfy` not . null

        it "can get module info" $ do
            withLightHscEnv [] $ \env ->
              runLightGhc env $ do
                mdl <- findModule "Data.List" Nothing
                mmi <- getModuleInfo mdl
                liftIO $ isJust mmi `shouldBe` True


    describe "resolveModule" $ do
        it "Works when a module given as path uses CPP" $ do
            dir <- getCurrentDirectory
            print dir
            let srcDirs = [dir </> "test/data/target/src"]
            withLightHscEnv [] $ \env -> runNullLog $ do
                Just _ <- resolveModule env srcDirs (Left $ dir </> "test/data/target/src/A/B/C/D/E.hs")
                return ()
