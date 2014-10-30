module CradleSpec where

import Control.Applicative
import Data.List (isSuffixOf)
import Language.Haskell.GhcMod.Cradle
import Language.Haskell.GhcMod.Types
import System.Directory (canonicalizePath,getCurrentDirectory)
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
                cradleCurrentDir res `shouldBe` curDir
                cradleRootDir    res `shouldBe` curDir
                cradleCabalFile  res `shouldBe` Nothing
                cradlePkgDbStack res `shouldBe` [GlobalDb,UserDb]

        it "finds a cabal file and a sandbox" $ do
            cwd <- getCurrentDirectory
            withDirectory "test/data/subdir1/subdir2" $ \dir -> do
                res <- relativeCradle dir <$> findCradle
                cradleCurrentDir res `shouldBe` "test" </> "data" </> "subdir1" </> "subdir2"
                cradleRootDir    res `shouldBe` "test" </> "data"
                cradleCabalFile  res `shouldBe` Just ("test" </> "data" </> "cabalapi.cabal")
                cradlePkgDbStack res `shouldBe` [GlobalDb, PackageDb (cwd </> "test/data/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d")]

        it "works even if a sandbox config file is broken" $ do
            withDirectory "test/data/broken-sandbox" $ \dir -> do
                res <- relativeCradle dir <$> findCradle
                cradleCurrentDir res `shouldBe` "test" </> "data" </> "broken-sandbox"
                cradleRootDir    res `shouldBe` "test" </> "data" </> "broken-sandbox"
                cradleCabalFile  res `shouldBe` Just ("test" </> "data" </> "broken-sandbox" </> "dummy.cabal")
                cradlePkgDbStack res `shouldBe` [GlobalDb, UserDb]

relativeCradle :: FilePath -> Cradle -> Cradle
relativeCradle dir cradle = cradle {
    cradleCurrentDir    = toRelativeDir dir  $  cradleCurrentDir cradle
  , cradleRootDir       = toRelativeDir dir  $  cradleRootDir    cradle
  , cradleCabalFile     = toRelativeDir dir <$> cradleCabalFile  cradle
  }

-- Work around GHC 7.2.2 where `canonicalizePath "/"` returns "/.".
stripLastDot :: FilePath -> FilePath
stripLastDot path
  | (pathSeparator:'.':"") `isSuffixOf` path = init path
  | otherwise = path
