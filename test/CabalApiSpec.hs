module CabalApiSpec where

import Test.Hspec
import CabalApi

spec :: Spec
spec = do
    describe "cabalDependPackages" $ do
        it "extracts dependent packages" $ do
            pkgs <- cabalParseFile "test/data/cabalapi.cabal" >>= cabalDependPackages
            pkgs `shouldBe` ["Cabal","base","containers","convertible","directory","filepath","ghc","ghc-paths","ghc-syb-utils","hlint","hspec","io-choice","old-time","process","regex-posix","syb","time","transformers"]

    describe "cabalBuildInfo" $ do
        it "extracts build info" $ do
            info <- cabalParseFile "test/data/cabalapi.cabal" >>= cabalBuildInfo
            let infoStr = show info
            infoStr `shouldBe` "BuildInfo {buildable = True, buildTools = [], cppOptions = [], ccOptions = [], ldOptions = [], pkgconfigDepends = [], frameworks = [], cSources = [], hsSourceDirs = [], otherModules = [ModuleName [\"Browse\"],ModuleName [\"CabalApi\"],ModuleName [\"Cabal\"],ModuleName [\"CabalDev\"],ModuleName [\"Check\"],ModuleName [\"ErrMsg\"],ModuleName [\"Flag\"],ModuleName [\"GHCApi\"],ModuleName [\"GHCChoice\"],ModuleName [\"Gap\"],ModuleName [\"Info\"],ModuleName [\"Lang\"],ModuleName [\"Lint\"],ModuleName [\"List\"],ModuleName [\"Paths_ghc_mod\"],ModuleName [\"Types\"]], defaultLanguage = Nothing, otherLanguages = [], defaultExtensions = [], otherExtensions = [], oldExtensions = [], extraLibs = [], extraLibDirs = [], includeDirs = [], includes = [], installIncludes = [], options = [(GHC,[\"-Wall\"])], ghcProfOptions = [], ghcSharedOptions = [], customFieldsBI = [], targetBuildDepends = []}"
