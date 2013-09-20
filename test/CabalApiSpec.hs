{-# LANGUAGE ScopedTypeVariables #-}

module CabalApiSpec where

import Control.Applicative
import Control.Exception
import Data.Maybe
import Language.Haskell.GhcMod.CabalApi
import Language.Haskell.GhcMod.Cradle
import Language.Haskell.GhcMod.Types
import Test.Hspec

import Dir

spec :: Spec
spec = do
    describe "parseCabalFile" $ do
        it "throws an exception if the cabal file is broken" $ do
            parseCabalFile "test/data/broken-cabal/broken.cabal" `shouldThrow` (\(_::IOException) -> True)

    describe "getCompilerOptions" $ do
        it "gets necessary CompilerOptions" $ do
            withDirectory "test/data/subdir1/subdir2" $ \dir -> do
                cradle <- findCradle
                pkgDesc <- parseCabalFile $ fromJust $ cradleCabalFile cradle
                res <- getCompilerOptions [] cradle pkgDesc
                let res' = res { includeDirs = map (toRelativeDir dir) (includeDirs res) }
                res' `shouldBe` CompilerOptions {ghcOptions = ["-XHaskell98"], includeDirs = ["test/data","test/data/dist/build","test/data/subdir1/subdir2","test/data/test"], depPackages = ["Cabal","base","template-haskell"]}

    describe "cabalDependPackages" $ do
        it "extracts dependent packages" $ do
            pkgs <- cabalDependPackages . cabalAllBuildInfo <$> parseCabalFile "test/data/cabalapi.cabal"
            pkgs `shouldBe` ["Cabal","base","template-haskell"]

    describe "cabalSourceDirs" $ do
        it "extracts all hs-source-dirs" $ do
            dirs <- cabalSourceDirs . cabalAllBuildInfo <$> parseCabalFile "test/data/check-test-subdir/check-test-subdir.cabal"
            dirs `shouldBe` ["src", "test"]
        it "extracts all hs-source-dirs including \".\"" $ do
            dirs <- cabalSourceDirs . cabalAllBuildInfo <$> parseCabalFile "test/data/cabalapi.cabal"
            dirs `shouldBe` [".", "test"]

    describe "cabalAllBuildInfo" $ do
        it "extracts build info" $ do
            info <- cabalAllBuildInfo <$> parseCabalFile "test/data/cabalapi.cabal"
            show info `shouldBe` "[BuildInfo {buildable = True, buildTools = [], cppOptions = [], ccOptions = [], ldOptions = [], pkgconfigDepends = [], frameworks = [], cSources = [], hsSourceDirs = [\".\"], otherModules = [ModuleName [\"Browse\"],ModuleName [\"CabalApi\"],ModuleName [\"Cabal\"],ModuleName [\"CabalDev\"],ModuleName [\"Check\"],ModuleName [\"ErrMsg\"],ModuleName [\"Flag\"],ModuleName [\"GHCApi\"],ModuleName [\"GHCChoice\"],ModuleName [\"Gap\"],ModuleName [\"Info\"],ModuleName [\"Lang\"],ModuleName [\"Lint\"],ModuleName [\"List\"],ModuleName [\"Paths_ghc_mod\"],ModuleName [\"Types\"]], defaultLanguage = Nothing, otherLanguages = [], defaultExtensions = [], otherExtensions = [], oldExtensions = [], extraLibs = [], extraLibDirs = [], includeDirs = [], includes = [], installIncludes = [], options = [(GHC,[\"-Wall\"])], ghcProfOptions = [], ghcSharedOptions = [], customFieldsBI = [], targetBuildDepends = [Dependency (PackageName \"Cabal\") (UnionVersionRanges (ThisVersion (Version {versionBranch = [1,10], versionTags = []})) (LaterVersion (Version {versionBranch = [1,10], versionTags = []}))),Dependency (PackageName \"base\") (IntersectVersionRanges (UnionVersionRanges (ThisVersion (Version {versionBranch = [4,0], versionTags = []})) (LaterVersion (Version {versionBranch = [4,0], versionTags = []}))) (EarlierVersion (Version {versionBranch = [5], versionTags = []}))),Dependency (PackageName \"template-haskell\") AnyVersion]},BuildInfo {buildable = True, buildTools = [], cppOptions = [], ccOptions = [], ldOptions = [], pkgconfigDepends = [], frameworks = [], cSources = [], hsSourceDirs = [\"test\",\".\"], otherModules = [ModuleName [\"Expectation\"],ModuleName [\"BrowseSpec\"],ModuleName [\"CabalApiSpec\"],ModuleName [\"FlagSpec\"],ModuleName [\"LangSpec\"],ModuleName [\"LintSpec\"],ModuleName [\"ListSpec\"]], defaultLanguage = Nothing, otherLanguages = [], defaultExtensions = [], otherExtensions = [], oldExtensions = [], extraLibs = [], extraLibDirs = [], includeDirs = [], includes = [], installIncludes = [], options = [], ghcProfOptions = [], ghcSharedOptions = [], customFieldsBI = [], targetBuildDepends = [Dependency (PackageName \"Cabal\") (UnionVersionRanges (ThisVersion (Version {versionBranch = [1,10], versionTags = []})) (LaterVersion (Version {versionBranch = [1,10], versionTags = []}))),Dependency (PackageName \"base\") (IntersectVersionRanges (UnionVersionRanges (ThisVersion (Version {versionBranch = [4,0], versionTags = []})) (LaterVersion (Version {versionBranch = [4,0], versionTags = []}))) (EarlierVersion (Version {versionBranch = [5], versionTags = []})))]}]"
