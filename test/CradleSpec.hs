module CradleSpec where

import Control.Applicative
import Data.List (isPrefixOf)
import Expectation
import Language.Haskell.GhcMod
import System.Directory (canonicalizePath)
import System.FilePath (addTrailingPathSeparator, (</>))
import Test.Hspec

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
                res <- relativeCradle dir <$> findCradle Nothing "7.6.2"
                res `shouldBe` Cradle {
                    cradleCurrentDir = "test" </> "data" </> "subdir1" </> "subdir2"
                  , cradleCabalDir = Just ("test" </> "data")
                  , cradleCabalFile = Just ("test" </> "data" </> "cabalapi.cabal")
                  , cradlePackageConf = Just ("test" </> "data" </> "cabal-dev" </> "packages-7.6.2.conf")
                  }

        it "finds a sandbox if exists" $ do
            withDirectory "/" $ \dir -> do
                curDir <- canonicalizePath "/"
                res <- relativeCradle dir <$> findCradle (Just $ addTrailingPathSeparator dir ++ ("test" </> "data" </> "cabal-dev")) "7.6.2"
                res `shouldBe` Cradle {
                    cradleCurrentDir = curDir
                  , cradleCabalDir = Nothing
                  , cradleCabalFile = Nothing
                  , cradlePackageConf = Just ("test" </> "data" </> "cabal-dev" </> "packages-7.6.2.conf")
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


toRelativeDir :: FilePath -> FilePath -> FilePath
toRelativeDir dir file
  | dir' `isPrefixOf` file = drop len file
  | otherwise              = file
  where
    dir' = addTrailingPathSeparator dir
    len = length dir'
