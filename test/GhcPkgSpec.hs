{-# LANGUAGE CPP #-}
module GhcPkgSpec where

import Language.Haskell.GhcMod.GhcPkg
import Language.Haskell.GhcMod.Types

import System.Directory
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = do
    describe "getSandboxDb" $ do
-- ghc < 7.8
#if !MIN_VERSION_ghc(7,8,0)
        it "does include a sandbox with ghc < 7.8" $ do
            cwd <- getCurrentDirectory
            getPackageDbStack "test/data/" `shouldReturn` [GlobalDb, PackageDb $ cwd </> "test/data/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d"]
#endif

        it "can parse a config file and extract the sandbox package-db" $ do
            cwd <- getCurrentDirectory
            pkgDb <- getSandboxDb "test/data/"
            pkgDb `shouldBe` (cwd </> "test/data/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d")

        it "throws an error if the sandbox config file is broken" $ do
            getSandboxDb "test/data/broken-sandbox" `shouldThrow` anyException
