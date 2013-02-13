module CabalSpec where

import Control.Applicative
import System.Directory
import Test.Hspec
import Cabal
import Expectation

spec :: Spec
spec = do
    describe "getDirs" $ do
        it "obtains two directories and a cabal file" $ do
            len <- length <$> getCurrentDirectory
            withDirectory "test/data/subdir1/subdir2" $ do
                (x,y,z) <- getDirs
                (drop len x, drop len y, drop len z)  `shouldBe` ("/test/data/subdir1/subdir2","/test/data","/test/data/cabalapi.cabal")

    describe "getDirs" $ do
        it "obtains two directories and a cabal file" $ do
            len <- length <$> getCurrentDirectory
            withDirectory "test/data/subdir1/subdir2" $ do
                (x,y,z) <- fromCabal []
                (x, map (drop len) y, z) `shouldBe` (["-XHaskell98"],["/test/data","/test/data/subdir1/subdir2"],["Cabal","base","containers","convertible","directory","filepath","ghc","ghc-paths","ghc-syb-utils","hlint","hspec","io-choice","old-time","process","regex-posix","syb","time","transformers"])
