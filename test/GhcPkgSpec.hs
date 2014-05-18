module GhcPkgSpec where

import Language.Haskell.GhcMod.GhcPkg

import System.Directory
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = do
    describe "getSandboxDb" $ do
        it "parses a config file and extracts sandbox package db" $ do
            cwd <- getCurrentDirectory
            pkgDb <- getSandboxDb "test/data/"
            pkgDb `shouldBe` (cwd </> "test/data/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d")

        it "throws an error if a config file is broken" $ do
            getSandboxDb "test/data/broken-sandbox" `shouldThrow` anyException
