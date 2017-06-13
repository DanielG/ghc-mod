module CradleSpec where

import Control.Applicative
import Data.List (isSuffixOf)
import GhcMod.Cradle
import GhcMod.Types
import System.Directory (canonicalizePath)
import System.FilePath (pathSeparator)
import Test.Hspec
import TestUtils
import Prelude

import Dir

clean_ :: IO Cradle -> IO Cradle
clean_ f = do
  crdl <- f
  cleanupCradle crdl
  return crdl

relativeCradle :: FilePath -> Cradle -> Cradle
relativeCradle dir crdl = crdl {
    cradleCurrentDir    = toRelativeDir dir  $  cradleCurrentDir crdl
  , cradleRootDir       = toRelativeDir dir  $  cradleRootDir    crdl
  , cradleCabalFile     = toRelativeDir dir <$> cradleCabalFile  crdl
  }

-- Work around GHC 7.2.2 where `canonicalizePath "/"` returns "/.".
stripLastDot :: FilePath -> FilePath
stripLastDot path
  | (pathSeparator:'.':"") `isSuffixOf` path = init path
  | otherwise = path

spec :: Spec
spec = do
    describe "findCradle" $ do
        it "returns the current directory" $ do
            withDirectory_ "/" $ do
                curDir <- stripLastDot <$> canonicalizePath "/"
                res <- clean_ $ runGmOutDef $ findCradleNoLog $ optPrograms defaultOptions
                cradleCurrentDir res `shouldBe` curDir
                cradleRootDir    res `shouldBe` curDir
                cradleCabalFile  res `shouldBe` Nothing

        it "finds a cabal file and a sandbox" $ do
            withDirectory "test/data/cabal-project/subdir1/subdir2" $ \dir -> do
                res <- relativeCradle dir <$> clean_ (runGmOutDef $ findCradleNoLog $ optPrograms defaultOptions)

                cradleCurrentDir res `shouldBe`
                    "test/data/cabal-project/subdir1/subdir2"

                cradleRootDir    res `shouldBe` "test/data/cabal-project"

                cradleCabalFile  res `shouldBe`
                    Just ("test/data/cabal-project/cabalapi.cabal")

        it "works even if a sandbox config file is broken" $ do
            withDirectory "test/data/broken-sandbox" $ \dir -> do
                res <- relativeCradle dir <$> clean_ (runGmOutDef $ findCradleNoLog $ optPrograms defaultOptions)
                cradleCurrentDir res `shouldBe`
                    "test" </> "data" </> "broken-sandbox"

                cradleRootDir    res `shouldBe`
                    "test" </> "data" </> "broken-sandbox"

                cradleCabalFile  res `shouldBe`
                  Just ("test" </> "data" </> "broken-sandbox" </> "dummy.cabal")
