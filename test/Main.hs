{-# LANGUAGE CPP, ScopedTypeVariables #-}
import Spec
import Dir

import Test.Hspec
import System.Process

import Language.Haskell.GhcMod (debugInfo, defaultOptions, findCradle)
import Control.Exception as E
import TestUtils

main = do
  let sandboxes = [ "test/data", "test/data/check-packageid"
                  , "test/data/duplicate-pkgver/"
                  , "test/data/broken-cabal/"
                  ]
      genSandboxCfg dir = withDirectory dir $ \cwd -> do
         system ("sed 's|@CWD@|" ++ cwd ++ "|g' cabal.sandbox.config.in > cabal.sandbox.config")
      pkgDirs =
        [ "test/data/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d"
        , "test/data/check-packageid/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d"
        , "test/data/duplicate-pkgver/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d"]
      genGhcPkgCache dir = system $ "ghc-pkg recache --force -f" ++ dir
  genSandboxCfg `mapM_` sandboxes
  genGhcPkgCache `mapM_` pkgDirs
  system "find test -name setup-config -name ghc-mod.cache -exec rm {} \\;"
  system "cabal --version"
  putStrLn $ "ghc-mod was built with Cabal version " ++ VERSION_Cabal
  system "ghc --version"

  (putStrLn =<< runD debugInfo)
      `E.catch` (\(_ :: E.SomeException) -> return () )

  hspec spec
