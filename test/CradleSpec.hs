module CradleSpec where

import Control.Applicative
import Language.Haskell.GhcMod
import System.Directory (canonicalizePath)
import System.FilePath (addTrailingPathSeparator, (</>))
import Test.Hspec

import Dir

spec :: Spec
spec = do
    describe "findCradle" $ do
        it "returns the current directory" $ do
            withDirectory_ "/" $ do
                curDir <- canonicalizePath "/"
                res <- findCradle Nothing "7.4.1"
                res `shouldBe` Cradle {
                    cradleCurrentDir = curDir
                  , cradleCabalDir = Nothing
                  , cradleCabalFile = Nothing
                  , cradlePackageConf = Nothing
                  }

        it "finds a cabal file" $ do
            withDirectory "test/data/subdir1/subdir2" $ \dir -> do
                res <- relativeCradle dir <$> findCradle Nothing "7.4.1"
                res `shouldBe` Cradle {
                    cradleCurrentDir = "test" </> "data" </> "subdir1" </> "subdir2"
                  , cradleCabalDir = Just ("test" </> "data")
                  , cradleCabalFile = Just ("test" </> "data" </> "cabalapi.cabal")
                  , cradlePackageConf = Nothing
                  }

        it "finds a sandbox" $ do
            withDirectory "test/data/subdir1/subdir2" $ \dir -> do
                res <- relativeCradle dir <$> findCradle Nothing "7.6.3"
                res `shouldBe` Cradle {
                    cradleCurrentDir = "test" </> "data" </> "subdir1" </> "subdir2"
                  , cradleCabalDir = Just ("test" </> "data")
                  , cradleCabalFile = Just ("test" </> "data" </> "cabalapi.cabal")
                  , cradlePackageConf = Just ("test" </> "data" </> ".cabal-sandbox" </> "i386-osx-ghc-7.6.3-packages.conf.d")
                  }

        it "finds a sandbox if exists" $ do
            withDirectory "/" $ \dir -> do
                curDir <- canonicalizePath "/"
                res <- relativeCradle dir <$> findCradle (Just $ addTrailingPathSeparator dir ++ ("test" </> "data" </> ".cabal-sandbox")) "7.6.3"
                res `shouldBe` Cradle {
                    cradleCurrentDir = curDir
                  , cradleCabalDir = Nothing
                  , cradleCabalFile = Nothing
                  , cradlePackageConf = Just ("test" </> "data" </> ".cabal-sandbox" </> "i386-osx-ghc-7.6.3-packages.conf.d")
                  }

        it "throws an error if the sandbox does not exist" $ do
            withDirectory_ "/" $
                findCradle (Just "/") "7.4.1" `shouldThrow` anyIOException

relativeCradle :: FilePath -> Cradle -> Cradle
relativeCradle dir cradle = Cradle {
    cradleCurrentDir  = toRelativeDir dir  $  cradleCurrentDir  cradle
  , cradleCabalDir    = toRelativeDir dir <$> cradleCabalDir    cradle
  , cradleCabalFile   = toRelativeDir dir <$> cradleCabalFile   cradle
  , cradlePackageConf = toRelativeDir dir <$> cradlePackageConf cradle
  }
