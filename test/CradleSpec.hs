module CradleSpec where

import Control.Applicative
import Data.List (isSuffixOf)
import Language.Haskell.GhcMod.Cradle
import Language.Haskell.GhcMod.Types
import System.Directory (canonicalizePath,getCurrentDirectory)
import System.FilePath ((</>), pathSeparator)
import Test.Hspec

import Dir
import TestUtils

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
                res <- clean_ findCradle
                cradleCurrentDir res `shouldBe` curDir
                cradleRootDir    res `shouldBe` curDir
                cradleCabalFile  res `shouldBe` Nothing
                cradlePkgDbStack res `shouldBe` [GlobalDb,UserDb]

        it "finds a cabal file and a sandbox" $ do
            cwd <- getCurrentDirectory
            withDirectory "test/data/cabal-project/subdir1/subdir2" $ \dir -> do
                res <- relativeCradle dir <$> clean_ findCradle

                cradleCurrentDir res `shouldBe`
                    "test/data/cabal-project/subdir1/subdir2"

                cradleRootDir    res `shouldBe` "test/data/cabal-project"

                cradleCabalFile  res `shouldBe`
                    Just ("test/data/cabal-project/cabalapi.cabal")

                let [GlobalDb, sb] = cradlePkgDbStack res
                sb `shouldSatisfy`
                   isPkgDbAt (cwd </> "test/data/cabal-project/.cabal-sandbox")

        it "works even if a sandbox config file is broken" $ do
            withDirectory "test/data/broken-sandbox" $ \dir -> do
                res <- relativeCradle dir <$> clean_ findCradle
                cradleCurrentDir res `shouldBe`
                    "test" </> "data" </> "broken-sandbox"

                cradleRootDir    res `shouldBe`
                    "test" </> "data" </> "broken-sandbox"


                cradleCabalFile  res `shouldBe`
                  Just ("test" </> "data" </> "broken-sandbox" </> "dummy.cabal")

                cradlePkgDbStack res `shouldBe` [GlobalDb, UserDb]

        it "uses the custom cradle file if present" $ do
            withDirectory "test/data/custom-cradle" $ \dir -> do
                res <- relativeCradle dir <$> findCradle
                cradleCurrentDir res `shouldBe` "test" </> "data" </> "custom-cradle"
                cradleRootDir res    `shouldBe` "test" </> "data" </> "custom-cradle"
                cradleCabalFile res  `shouldBe` Just ("test" </> "data" </> "custom-cradle" </> "dummy.cabal")
                cradlePkgDbStack res `shouldBe` [PackageDb "a/packages", GlobalDb, PackageDb "b/packages", UserDb, PackageDb "c/packages"]
