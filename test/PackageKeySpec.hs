{-# LANGUAGE CPP #-}
module PackageKeySpec where

import Language.Haskell.GhcMod
import qualified Language.Haskell.GhcMod.Gap as Gap
import Language.Haskell.GhcMod.Logger

import qualified DynFlags      as GHC
import qualified GHC           as GHC
import qualified Module        as GHC
import qualified Outputable    as GHC

import Data.List
import System.Directory
import System.Process
import Test.Hspec

import TestUtils
import Dir

main :: IO ()
main = do
  hspec spec

{-

Can be run in ghci (via inf-haskell-mode) by doing the following in the inferior *haskell* session

:set -package ghc
:set -XFlexibleContexts

Reload the file (CTRL-l)

:cd ".."
:main


-}
spec :: Spec
spec = do
    describe "checkPackageKey" $ do
        it "main module import packageKey should not be 'main@main'" $ do
            withDirectory_ "test/data/cabal-packagekey" $ do
                res <- runD $ checkPackageKeys
                res `shouldNotBe` "[(main@main:Main, Main.baz), (main@main:Foo.Baz, Foo.Baz.bar),\n (main@main:Foo.Bar, Foo.Bar.bar)]"

-- ---------------------------------------------------------------------

checkPackageKeys :: IOish m => GhcModT m String
checkPackageKeys =
    runGmlTWith
      [Left "./src/Main.hs"]
      return
      id
      action
  where
    action = do
      graph  <- GHC.getModuleGraph
      let modSum = head graph

      setGhcContext modSum
      gnames <- GHC.getNamesInScope
      let problematic = take 3 $ reverse gnames

      let eqModules (GHC.Module pk1 mn1) (GHC.Module pk2 mn2) = mn1 == mn2

      return  $ showGhcQual (map (\n -> (GHC.nameModule n,n)) problematic)

showGhcQual :: (GHC.Outputable a) => a -> String
showGhcQual x = GHC.showSDocForUser GHC.unsafeGlobalDynFlags GHC.alwaysQualify $ GHC.ppr x
setGhcContext :: GHC.GhcMonad m => GHC.ModSummary -> m ()

#if __GLASGOW_HASKELL__ > 704
setGhcContext modSum = GHC.setContext [GHC.IIModule (GHC.moduleName $ GHC.ms_mod modSum)]
#else
setGhcContext modSum = GHC.setContext [GHC.IIModule (                 GHC.ms_mod modSum)]
#endif

pwd :: IO FilePath
pwd = getCurrentDirectory

cd :: FilePath -> IO ()
cd = setCurrentDirectory
