module GhcApiSpec where

import Control.Applicative
import Control.Monad
import Data.List (sort)
import Language.Haskell.GhcMod.GHCApi
import Test.Hspec
import TestUtils
import CoreMonad (liftIO)

import Dir

spec :: Spec
spec = do
    describe "findModule" $ do
        it "finds Data.List in `base' and `haskell2010'"
            $ withDirectory_ "test/data" $ runD $ do
                pkgs <- findModule "Data.List" <$> ghcPkgDb
                let pkgNames = pkgName `map` pkgs
                liftIO $ pkgNames `shouldContain` ["base", "haskell2010"]

    describe "moduleInfo" $ do
        it "works for modules from global packages (e.g. base:Data.List)"
            $ withDirectory_ "test/data" $ runD $ do
                Just info <- moduleInfo (Just ("base","","")) "Data.List"
                liftIO $ sort (bindings info) `shouldContain` ["++"]

        it "works for local modules"
            $ withDirectory_ "test/data" $ runD $ do
                Just info <- moduleInfo Nothing "Baz"
                liftIO $ bindings info `shouldContain` ["baz"]
