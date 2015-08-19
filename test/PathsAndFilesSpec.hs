module PathsAndFilesSpec where

import Language.Haskell.GhcMod.PathsAndFiles

import System.Directory
import System.FilePath
import Test.Hspec
import TestUtils

spec :: Spec
spec = do
    describe "getSandboxDb" $ do
        it "can parse a config file and extract the sandbox package-db" $ do
            cwd <- getCurrentDirectory
            Just db <- getSandboxDb "test/data/cabal-project"
            db `shouldSatisfy` isPkgDbAt (cwd </> "test/data/cabal-project/.cabal-sandbox")

        it "returns Nothing if the sandbox config file is broken" $ do
            getSandboxDb "test/data/broken-sandbox" `shouldReturn` Nothing

    describe "findCabalFile" $ do
        it "works" $ do
            findCabalFile "test/data/cabal-project" `shouldReturn` Just "test/data/cabal-project/cabalapi.cabal"

        it "finds cabal files in parent directories" $ do
            findCabalFile "test/data/cabal-project/subdir1/subdir2" `shouldReturn` Just "test/data/cabal-project/cabalapi.cabal"

    describe "findStackConfigFile" $ do
        it "works" $ do
            findStackConfigFile "test/data/stack-project" `shouldReturn` Just "test/data/stack-project/stack.yaml"

    describe "findCabalSandboxDir" $ do
        it "works" $ do
            findCabalSandboxDir "test/data/cabal-project" `shouldReturn` Just "test/data/cabal-project"

        it "finds sandboxes in parent directories" $ do
            findCabalSandboxDir "test/data/cabal-project/subdir1/subdir2" `shouldReturn` Just "test/data/cabal-project"
