{-# LANGUAGE CPP, ScopedTypeVariables #-}
import Spec
import Dir

import Control.Exception as E
import Control.Monad (void)
import Language.Haskell.GhcMod (debugInfo)
import System.Process
import Test.Hspec
import TestUtils

main :: IO ()
main = do
  let sandboxes = [ "test/data"
                  , "test/data/another-sandbox"
                  , "test/data/check-packageid"
                  , "test/data/duplicate-pkgver/"
                  , "test/data/broken-cabal/"
                  ]
      genSandboxCfg dir = withDirectory dir $ \cwdir ->
         system ("sed 's|@CWD@|" ++ cwdir ++ "|g' cabal.sandbox.config.in > cabal.sandbox.config")
      pkgDirs =
        [ "test/data/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d"
        , "test/data/another-sandbox/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d"
        , "test/data/check-packageid/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d"
        , "test/data/duplicate-pkgver/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d"
        ]
      genGhcPkgCache dir = system $ "ghc-pkg recache --force -f" ++ dir
  genSandboxCfg `mapM_` sandboxes
  genGhcPkgCache `mapM_` pkgDirs
  void $ system "find test -name setup-config -name ghc-mod.cache -exec rm {} \\;"
  void $ system "cabal --version"
  putStrLn $ "ghc-mod was built with Cabal version " ++ VERSION_Cabal
  void $ system "ghc --version"

  (putStrLn =<< runD debugInfo)
      `E.catch` (\(_ :: E.SomeException) -> return () )

  hspec spec
