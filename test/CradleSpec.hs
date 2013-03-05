module CradleSpec where

import Cradle
import Expectation
import System.Directory
import Test.Hspec
import Types

spec :: Spec
spec = do
    describe "findCradle" $ do
        it "returns the current directory" $ do
            withDirectory_ "/" $
                findCradle Nothing "7.4.1" `shouldReturn` Cradle {cradleCurrentDir = "/", cradleCabalDir = Nothing, cradleCabalFile = Nothing, cradlePackageConf = Nothing}

        it "finds a cabal file" $ do
            withDirectory "test/data/subdir1/subdir2" $ \dir ->
                findCradle Nothing "7.4.1" `shouldReturn` Cradle {cradleCurrentDir = "/Users/kazu/work/ghc-mod/test/data", cradleCabalDir = Just "/Users/kazu/work/ghc-mod/test/data", cradleCabalFile = Just "/Users/kazu/work/ghc-mod/test/data/cabalapi.cabal", cradlePackageConf = Nothing}

        it "finds a sandbox" $ do
            withDirectory "test/data/subdir1/subdir2" $ \dir ->
            	findCradle Nothing "7.6.2" `shouldReturn` Cradle {cradleCurrentDir = "/Users/kazu/work/ghc-mod/test/data", cradleCabalDir = Just "/Users/kazu/work/ghc-mod/test/data", cradleCabalFile = Just "/Users/kazu/work/ghc-mod/test/data/cabalapi.cabal", cradlePackageConf = Just "/Users/kazu/work/ghc-mod/test/data/cabal-dev/packages-7.6.2.conf"}

        it "finds a sandbox if exists" $ do
            withDirectory "/" $  \dir ->
                findCradle (Just "/Users/kazu/work/ghc-mod/test/data/cabal-dev") "7.6.2" `shouldReturn` Cradle {cradleCurrentDir = "/", cradleCabalDir = Nothing, cradleCabalFile = Nothing, cradlePackageConf = Just "/Users/kazu/work/ghc-mod/test/data/cabal-dev/packages-7.6.2.conf"}

