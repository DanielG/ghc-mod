module CradleSpec where

import Control.Applicative
import Data.List (isSuffixOf)
import Language.Haskell.GhcMod.Cradle
import Language.Haskell.GhcMod.Types
import System.Directory (canonicalizePath)
import System.FilePath ((</>), pathSeparator)
import Test.Hspec

import Dir

spec :: Spec
spec = do
    describe "findCradle" $ do
        it "returns the current directory" $ do
            withDirectory_ "/" $ do
                curDir <- stripLastDot <$> canonicalizePath "/"
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
                  , cradlePackageDbOpts = ["-no-user-package-db", "-package-db", "test" </> "data" </> ".cabal-sandbox" </> "/home/me/work/ghc-mod/test/data/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d"]
                  }
        it "works even if a sandbox config file is broken" $ do
            withDirectory "test/data/broken-sandbox" $ \dir -> do
                res <- relativeCradle dir <$> findCradle
                res `shouldBe` Cradle {
                    cradleCurrentDir    = "test" </> "data" </> "broken-sandbox"
                  , cradleCabalDir      = Just ("test" </> "data" </> "broken-sandbox")
                  , cradleCabalFile     = Just ("test" </> "data" </> "broken-sandbox" </> "dummy.cabal")
                  , cradlePackageDbOpts = []
                  }

    describe "getPackageDbDir" $ do
        it "parses a config file and extracts package db" $ do
            pkgDb <- getPackageDbDir "test/data/cabal.sandbox.config"
            pkgDb `shouldBe` "/home/me/work/ghc-mod/test/data/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d"

        it "throws an error if a config file is broken" $ do
            getPackageDbDir "test/data/bad.config" `shouldThrow` anyException

relativeCradle :: FilePath -> Cradle -> Cradle
relativeCradle dir cradle = Cradle {
    cradleCurrentDir    = toRelativeDir dir  $  cradleCurrentDir    cradle
  , cradleCabalDir      = toRelativeDir dir <$> cradleCabalDir      cradle
  , cradleCabalFile     = toRelativeDir dir <$> cradleCabalFile     cradle
  , cradlePackageDbOpts = cradlePackageDbOpts cradle
  }

-- Work around GHC 7.2.2 where `canonicalizePath "/"` returns "/.".
stripLastDot :: FilePath -> FilePath
stripLastDot path
  | (pathSeparator:'.':"") `isSuffixOf` path = init path
  | otherwise = path
