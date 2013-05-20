module CabalApiSpec where

import Control.Applicative
import Language.Haskell.GhcMod.CabalApi
import Test.Hspec

spec :: Spec
spec = do
    describe "cabalAllDependPackages" $ do
        it "extracts dependent packages" $ do
            pkgs <- cabalAllDependPackages <$> cabalParseFile "test/data/cabalapi.cabal"
            pkgs `shouldBe` ["Cabal","base","template-haskell"]

    describe "cabalAllSourceDirs" $ do
        it "extracts all hs-source-dirs" $ do
            dirs <- cabalAllSourceDirs <$> cabalParseFile "test/data/check-test-subdir/check-test-subdir.cabal"
            dirs `shouldBe` ["src", "test"]

    describe "cabalBuildInfo" $ do
        it "extracts build info" $ do
            info <- cabalBuildInfo <$> cabalParseFile "test/data/cabalapi.cabal"
            let infoStr = show info
            infoStr `shouldBe` "Just (BuildInfo {buildable = True, buildTools = [], cppOptions = [], ccOptions = [], ldOptions = [], pkgconfigDepends = [], frameworks = [], cSources = [], hsSourceDirs = [], otherModules = [ModuleName [\"Browse\"],ModuleName [\"CabalApi\"],ModuleName [\"Cabal\"],ModuleName [\"CabalDev\"],ModuleName [\"Check\"],ModuleName [\"ErrMsg\"],ModuleName [\"Flag\"],ModuleName [\"GHCApi\"],ModuleName [\"GHCChoice\"],ModuleName [\"Gap\"],ModuleName [\"Info\"],ModuleName [\"Lang\"],ModuleName [\"Lint\"],ModuleName [\"List\"],ModuleName [\"Paths_ghc_mod\"],ModuleName [\"Types\"]], defaultLanguage = Nothing, otherLanguages = [], defaultExtensions = [], otherExtensions = [], oldExtensions = [], extraLibs = [], extraLibDirs = [], includeDirs = [], includes = [], installIncludes = [], options = [(GHC,[\"-Wall\"])], ghcProfOptions = [], ghcSharedOptions = [], customFieldsBI = [], targetBuildDepends = []})"

        it "returns Nothing if the cabal file is broken" $ do
            info <- cabalBuildInfo <$> cabalParseFile "test/data/broken-cabal/broken.cabal"
            info `shouldBe` Nothing
