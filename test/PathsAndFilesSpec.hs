{-# LANGUAGE CPP #-}
module PathsAndFilesSpec where

import Language.Haskell.GhcMod.PathsAndFiles
#if __GLASGOW_HASKELL__ <= 706
import Language.Haskell.GhcMod.GhcPkg
#endif

import System.Directory
import System.FilePath
import Test.Hspec
import TestUtils

spec :: Spec
spec = do
    describe "getSandboxDb" $ do
-- ghc < 7.8
#if __GLASGOW_HASKELL__ <= 706
        it "does include a sandbox with ghc < 7.8" $ do
           cwd <- getCurrentDirectory
           [GlobalDb, sbPkgDb] <- getPackageDbStack "test/data/"
           sbPkgDb `shouldSatisfy` isPkgDbAt (cwd </> "test/data/.cabal-sandbox")
#endif

        it "can parse a config file and extract the sandbox package-db" $ do
            cwd <- getCurrentDirectory
            Just db <- getSandboxDb "test/data/"
            db `shouldSatisfy` isPkgDbAt (cwd </> "test/data/.cabal-sandbox")

        it "returns Nothing if the sandbox config file is broken" $ do
            getSandboxDb "test/data/broken-sandbox" `shouldReturn` Nothing

    describe "findCabalFile" $ do
        it "works" $ do
            findCabalFile "test/data" `shouldReturn` Just "test/data/cabalapi.cabal"

        it "finds cabal files in parent directories" $ do
            findCabalFile "test/data/subdir1/subdir2" `shouldReturn` Just "test/data/cabalapi.cabal"

    describe "findCabalSandboxDir" $ do
        it "works" $ do
            findCabalSandboxDir "test/data" `shouldReturn` Just "test/data"

        it "finds sandboxes in parent directories" $ do
            findCabalSandboxDir "test/data/subdir1/subdir2" `shouldReturn` Just "test/data"
