{-# LANGUAGE ScopedTypeVariables #-}

module CabalApiSpec where

import Control.Applicative
import Language.Haskell.GhcMod.CabalApi
import Language.Haskell.GhcMod.Cradle
import Language.Haskell.GhcMod.Types
import Test.Hspec
import System.Directory
import System.FilePath

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
                let Just cabalFile = cradleCabalFile crdl
                pkgDesc <- runD $ parseCabalFile crdl cabalFile
                res <- runD $ getCompilerOptions [] crdl pkgDesc
                let res' = res {
                        ghcOptions  = ghcOptions res
                      , includeDirs = map (toRelativeDir dir) (includeDirs res)
                      }

                let [fGlobalPkg, fNoUserPkg, fPkg, sb, _] = ghcOptions res'

                sb `shouldSatisfy`
                   isPkgConfDAt (cwd </> "test/data/.cabal-sandbox")

                if ghcVersion < 706
                  then do
                    fGlobalPkg `shouldBe` "-global-package-conf"
                    fNoUserPkg `shouldBe` "-no-user-package-conf"
                    fPkg `shouldBe` "-package-conf"

                  else do
                    fGlobalPkg `shouldBe` "-global-package-db"
                    fNoUserPkg `shouldBe` "-no-user-package-db"
                    fPkg `shouldBe` "-package-db"

                includeDirs res' `shouldBe` [
                                     "test/data",
                                     "test/data/dist/build",
                                     "test/data/dist/build/autogen",
                                     "test/data/subdir1/subdir2",
                                     "test/data/test"]

                (pkgName `map` depPackages res') `shouldContain` ["Cabal"]

    describe "cabalSourceDirs" $ do
        it "extracts all hs-source-dirs" $ do
            crdl <- findCradle' "test/data/check-test-subdir"
            let cabalFile = "test/data/check-test-subdir/check-test-subdir.cabal"
            dirs <- cabalSourceDirs . cabalAllBuildInfo
                    <$> runD (parseCabalFile crdl cabalFile)

            dirs `shouldBe` ["src", "test"]

        it "extracts all hs-source-dirs including \".\"" $ do
            crdl <- findCradle' "test/data/"
            dirs <- cabalSourceDirs . cabalAllBuildInfo
                    <$> runD (parseCabalFile crdl "test/data/cabalapi.cabal")
            dirs `shouldBe` [".", "test"]

    describe "cabalAllBuildInfo" $ do
        it "extracts build info" $ do
            crdl <- findCradle' "test/data/"
            info <- cabalAllBuildInfo <$> runD (parseCabalFile crdl "test/data/cabalapi.cabal")
            show info `shouldBe` "[BuildInfo {buildable = True, buildTools = [], cppOptions = [], ccOptions = [], ldOptions = [], pkgconfigDepends = [], frameworks = [], cSources = [], hsSourceDirs = [\".\"], otherModules = [ModuleName [\"Browse\"],ModuleName [\"CabalApi\"],ModuleName [\"Cabal\"],ModuleName [\"CabalDev\"],ModuleName [\"Check\"],ModuleName [\"ErrMsg\"],ModuleName [\"Flag\"],ModuleName [\"GHCApi\"],ModuleName [\"GHCChoice\"],ModuleName [\"Gap\"],ModuleName [\"Info\"],ModuleName [\"Lang\"],ModuleName [\"Lint\"],ModuleName [\"List\"],ModuleName [\"Paths_ghc_mod\"],ModuleName [\"Types\"]], defaultLanguage = Nothing, otherLanguages = [], defaultExtensions = [], otherExtensions = [], oldExtensions = [], extraLibs = [], extraLibDirs = [], includeDirs = [], includes = [], installIncludes = [], options = [(GHC,[\"-Wall\"])], ghcProfOptions = [], ghcSharedOptions = [], customFieldsBI = [], targetBuildDepends = [Dependency (PackageName \"Cabal\") (UnionVersionRanges (ThisVersion (Version {versionBranch = [1,10], versionTags = []})) (LaterVersion (Version {versionBranch = [1,10], versionTags = []}))),Dependency (PackageName \"base\") (IntersectVersionRanges (UnionVersionRanges (ThisVersion (Version {versionBranch = [4,0], versionTags = []})) (LaterVersion (Version {versionBranch = [4,0], versionTags = []}))) (EarlierVersion (Version {versionBranch = [5], versionTags = []}))),Dependency (PackageName \"template-haskell\") AnyVersion]},BuildInfo {buildable = True, buildTools = [], cppOptions = [], ccOptions = [], ldOptions = [], pkgconfigDepends = [], frameworks = [], cSources = [], hsSourceDirs = [\"test\",\".\"], otherModules = [ModuleName [\"Expectation\"],ModuleName [\"BrowseSpec\"],ModuleName [\"CabalApiSpec\"],ModuleName [\"FlagSpec\"],ModuleName [\"LangSpec\"],ModuleName [\"LintSpec\"],ModuleName [\"ListSpec\"]], defaultLanguage = Nothing, otherLanguages = [], defaultExtensions = [], otherExtensions = [], oldExtensions = [], extraLibs = [], extraLibDirs = [], includeDirs = [], includes = [], installIncludes = [], options = [], ghcProfOptions = [], ghcSharedOptions = [], customFieldsBI = [], targetBuildDepends = [Dependency (PackageName \"Cabal\") (UnionVersionRanges (ThisVersion (Version {versionBranch = [1,10], versionTags = []})) (LaterVersion (Version {versionBranch = [1,10], versionTags = []}))),Dependency (PackageName \"base\") (IntersectVersionRanges (UnionVersionRanges (ThisVersion (Version {versionBranch = [4,0], versionTags = []})) (LaterVersion (Version {versionBranch = [4,0], versionTags = []}))) (EarlierVersion (Version {versionBranch = [5], versionTags = []})))]}]"
