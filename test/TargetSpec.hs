{-# LANGUAGE OverloadedStrings #-}
module TargetSpec where

import GhcMod.Target
import GhcMod.LightGhc
import GhcMod.Gap
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
            (withLightHscEnv [] $ \env ->
              runLightGhc env $ do
               dflags <- getSessionDynFlags
               let i = intersect (listVisibleModuleNames dflags)
                                 ["Control.Applicative", "Control.Arrow"
                                 ,"Control.Exception", "GHC.Exts", "GHC.Float"]
               liftIO $ i `shouldSatisfy` not . null) :: IO ()

        it "can get module info" $ do
            (withLightHscEnv [] $ \env ->
              runLightGhc env $ do
                mdl <- findModule "Data.List" Nothing
                mmi <- getModuleInfo mdl
                liftIO $ isJust mmi `shouldBe` True) :: IO ()


    describe "resolveModule" $ do
        it "Works when a module given as path uses CPP" $ do
            dir <- getCurrentDirectory
            let srcDirs = [dir </> "test/data/target/src"]
            x <- withLightHscEnv [] $ \env -> runD $ do
                resolveModule env srcDirs (Left $ dir </> "test/data/target/Cpp.hs")
            liftIO $ x `shouldBe` Just (ModulePath "Cpp" $ dir </> "test/data/target/Cpp.hs")
