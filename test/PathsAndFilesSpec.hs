module PathsAndFilesSpec where


import GhcMod.PathsAndFiles
import GhcMod.Cradle
import qualified GhcMod.Utils as U

import Control.Monad.Trans.Maybe
import System.Directory
import System.FilePath
import Test.Hspec
import TestUtils

spec :: Spec
spec = do
    describe "getSandboxDb" $ do
        it "can parse a config file and extract the sandbox package-db" $ do
            cwd <- getCurrentDirectory
            Just crdl <- runLogDef $ runMaybeT $ plainCradle "test/data/cabal-project"
            Just db <- getSandboxDb crdl
            db `shouldSatisfy` isPkgDbAt (cwd </> "test/data/cabal-project/.cabal-sandbox")

        it "returns Nothing if the sandbox config file is broken" $ do
            Just crdl <- runLogDef $ runMaybeT $ plainCradle "test/data/broken-sandbox"
            getSandboxDb crdl  `shouldReturn` Nothing

    describe "findCabalFile" $ do
        it "works" $ do
            p <- U.makeAbsolute' "test/data/cabal-project/cabalapi.cabal"
            findCabalFile "test/data/cabal-project" `shouldReturn` Just p

        it "finds cabal files in parent directories" $ do
            p <- U.makeAbsolute' "test/data/cabal-project/cabalapi.cabal"
            findCabalFile "test/data/cabal-project/subdir1/subdir2" `shouldReturn` Just p

    describe "findStackConfigFile" $ do
        it "works" $ do
            p <- U.makeAbsolute' "test/data/stack-project/stack.yaml"
            findStackConfigFile "test/data/stack-project" `shouldReturn` Just p

    describe "findCabalSandboxDir" $ do
        it "works" $ do
            p <- U.makeAbsolute' "test/data/cabal-project"
            findCabalSandboxDir "test/data/cabal-project" `shouldReturn` Just p

        it "finds sandboxes in parent directories" $ do
            p <- U.makeAbsolute' "test/data/cabal-project"
            findCabalSandboxDir "test/data/cabal-project/subdir1/subdir2" `shouldReturn` Just p
