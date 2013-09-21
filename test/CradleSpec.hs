module CradleSpec where

import Control.Applicative
import Language.Haskell.GhcMod.Cradle
import Language.Haskell.GhcMod.Types
import System.Directory (canonicalizePath)
import System.FilePath ((</>))
import Test.Hspec

import Dir

spec :: Spec
spec = do
    describe "findCradle" $ do
        it "returns the current directory" $ do
            withDirectory_ "/" $ do
                curDir <- canonicalizePath "/"
                res <- findCradle
                res `shouldBe` Cradle {
                    cradleCurrentDir    = curDir
                  , cradleCabalDir      = Nothing
                  , cradleCabalFile     = Nothing
                  , cradlePackageDbOpts = []
                  }

        it "finds a cabal file and a sandbox" $ do
            withDirectory "test/data/subdir1/subdir2" $ \dir -> do
                res <- relativeCradle dir <$> findCradle
                res `shouldBe` Cradle {
                    cradleCurrentDir    = "test" </> "data" </> "subdir1" </> "subdir2"
                  , cradleCabalDir      = Just ("test" </> "data")
                  , cradleCabalFile     = Just ("test" </> "data" </> "cabalapi.cabal")
                  , cradlePackageDbOpts = ["-no-user-package-db", "-package-db", "test" </> "data" </> ".cabal-sandbox" </> "i386-osx-ghc-7.6.3-packages.conf.d"]
                  }
    describe "getPackageDbDir" $ do
        it "parses a config file and extracts package db" $ do
            pkgDb <- getPackageDbDir "test/data/cabal.sandbox.config"
            pkgDb `shouldBe` "/Users/kazu/work/ghc-mod/test/data/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d"

        it "throws an error if a config file is broken" $ do
            getPackageDbDir "test/data/bad.config" `shouldThrow` anyException

relativeCradle :: FilePath -> Cradle -> Cradle
relativeCradle dir cradle = Cradle {
    cradleCurrentDir    = toRelativeDir dir  $  cradleCurrentDir    cradle
  , cradleCabalDir      = toRelativeDir dir <$> cradleCabalDir      cradle
  , cradleCabalFile     = toRelativeDir dir <$> cradleCabalFile     cradle
  , cradlePackageDbOpts = map (toRelativeDir dir) (cradlePackageDbOpts cradle)
  }
