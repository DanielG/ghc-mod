{-# LANGUAGE ScopedTypeVariables #-}

module CabalApiSpec where

import Control.Applicative
import Data.Maybe
import Language.Haskell.GhcMod.CabalApi
import Language.Haskell.GhcMod.Cradle
import Language.Haskell.GhcMod.Types
import Test.Hspec
import System.Directory
import System.FilePath
import System.Process (readProcess)

import Dir
import TestUtils

import Config (cProjectVersionInt) -- ghc version

ghcVersion :: Int
ghcVersion = read cProjectVersionInt

spec :: Spec
spec = do
    describe "parseCabalFile" $ do
        it "throws an exception if the cabal file is broken" $ do
            shouldReturnError $ do
              withDirectory_ "test/data/broken-cabal" $ do
                  crdl <- findCradle
                  runD' $ parseCabalFile crdl "broken.cabal"


    describe "getCompilerOptions" $ do
        it "gets necessary CompilerOptions" $ do
            cwd <- getCurrentDirectory
            withDirectory "test/data/subdir1/subdir2" $ \dir -> do
                crdl <- findCradle
                pkgDesc <- runD $ parseCabalFile crdl $ fromJust $ cradleCabalFile crdl
                res <- runD $ getCompilerOptions [] crdl pkgDesc
                let res' = res {
                        ghcOptions  = ghcOptions res
                      , includeDirs = map (toRelativeDir dir) (includeDirs res)
                      }
                if ghcVersion < 706
                  then ghcOptions res' `shouldContain` ["-global-package-conf", "-no-user-package-conf","-package-conf",cwd </> "test/data/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d","-XHaskell98"]
                  else ghcOptions res' `shouldContain` ["-global-package-db", "-no-user-package-db","-package-db",cwd </> "test/data/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d","-XHaskell98"]
                includeDirs res' `shouldBe` ["test/data","test/data/dist/build","test/data/dist/build/autogen","test/data/subdir1/subdir2","test/data/test"]
                (pkgName `map` depPackages res') `shouldContain` ["Cabal"]


    describe "cabalDependPackages" $ do
        it "extracts dependent packages" $ do
            crdl <- findCradle' "test/data/"
            pkgs <- cabalDependPackages . cabalAllBuildInfo <$> runD (parseCabalFile crdl "test/data/cabalapi.cabal")
            pkgs `shouldBe` ["Cabal","base","template-haskell"]
        it "uses non default flags" $ do
            withDirectory_ "test/data/cabal-flags" $ do
                crdl <- findCradle
                _ <- readProcess "cabal" ["configure", "-ftest-flag"] ""
                pkgs <- cabalDependPackages . cabalAllBuildInfo <$> runD (parseCabalFile crdl "cabal-flags.cabal")
                pkgs `shouldBe` ["Cabal","base"]

    describe "cabalSourceDirs" $ do
        it "extracts all hs-source-dirs" $ do
            crdl <- findCradle' "test/data/check-test-subdir"
            dirs <- cabalSourceDirs . cabalAllBuildInfo <$> runD (parseCabalFile crdl "test/data/check-test-subdir/check-test-subdir.cabal")
            dirs `shouldBe` ["src", "test"]
        it "extracts all hs-source-dirs including \".\"" $ do
            crdl <- findCradle' "test/data/"
            dirs <- cabalSourceDirs . cabalAllBuildInfo <$> runD (parseCabalFile crdl "test/data/cabalapi.cabal")
            dirs `shouldBe` [".", "test"]

    describe "cabalAllBuildInfo" $ do
        it "extracts build info" $ do
            crdl <- findCradle' "test/data/"
            info <- cabalAllBuildInfo <$> runD (parseCabalFile crdl "test/data/cabalapi.cabal")
            show info `shouldBe` "[BuildInfo {buildable = True, buildTools = [], cppOptions = [], ccOptions = [], ldOptions = [], pkgconfigDepends = [], frameworks = [], cSources = [], hsSourceDirs = [\".\"], otherModules = [ModuleName [\"Browse\"],ModuleName [\"CabalApi\"],ModuleName [\"Cabal\"],ModuleName [\"CabalDev\"],ModuleName [\"Check\"],ModuleName [\"ErrMsg\"],ModuleName [\"Flag\"],ModuleName [\"GHCApi\"],ModuleName [\"GHCChoice\"],ModuleName [\"Gap\"],ModuleName [\"Info\"],ModuleName [\"Lang\"],ModuleName [\"Lint\"],ModuleName [\"List\"],ModuleName [\"Paths_ghc_mod\"],ModuleName [\"Types\"]], defaultLanguage = Nothing, otherLanguages = [], defaultExtensions = [], otherExtensions = [], oldExtensions = [], extraLibs = [], extraLibDirs = [], includeDirs = [], includes = [], installIncludes = [], options = [(GHC,[\"-Wall\"])], ghcProfOptions = [], ghcSharedOptions = [], customFieldsBI = [], targetBuildDepends = [Dependency (PackageName \"Cabal\") (UnionVersionRanges (ThisVersion (Version {versionBranch = [1,10], versionTags = []})) (LaterVersion (Version {versionBranch = [1,10], versionTags = []}))),Dependency (PackageName \"base\") (IntersectVersionRanges (UnionVersionRanges (ThisVersion (Version {versionBranch = [4,0], versionTags = []})) (LaterVersion (Version {versionBranch = [4,0], versionTags = []}))) (EarlierVersion (Version {versionBranch = [5], versionTags = []}))),Dependency (PackageName \"template-haskell\") AnyVersion]},BuildInfo {buildable = True, buildTools = [], cppOptions = [], ccOptions = [], ldOptions = [], pkgconfigDepends = [], frameworks = [], cSources = [], hsSourceDirs = [\"test\",\".\"], otherModules = [ModuleName [\"Expectation\"],ModuleName [\"BrowseSpec\"],ModuleName [\"CabalApiSpec\"],ModuleName [\"FlagSpec\"],ModuleName [\"LangSpec\"],ModuleName [\"LintSpec\"],ModuleName [\"ListSpec\"]], defaultLanguage = Nothing, otherLanguages = [], defaultExtensions = [], otherExtensions = [], oldExtensions = [], extraLibs = [], extraLibDirs = [], includeDirs = [], includes = [], installIncludes = [], options = [], ghcProfOptions = [], ghcSharedOptions = [], customFieldsBI = [], targetBuildDepends = [Dependency (PackageName \"Cabal\") (UnionVersionRanges (ThisVersion (Version {versionBranch = [1,10], versionTags = []})) (LaterVersion (Version {versionBranch = [1,10], versionTags = []}))),Dependency (PackageName \"base\") (IntersectVersionRanges (UnionVersionRanges (ThisVersion (Version {versionBranch = [4,0], versionTags = []})) (LaterVersion (Version {versionBranch = [4,0], versionTags = []}))) (EarlierVersion (Version {versionBranch = [5], versionTags = []})))]}]"
