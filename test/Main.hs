{-# LANGUAGE CPP, ScopedTypeVariables #-}
import Spec
import Dir

import Control.Exception as E
import Control.Monad (void)
import Data.List
import Language.Haskell.GhcMod (debugInfo)
import System.Process
import Test.Hspec
import TestUtils

main :: IO ()
main = do
  let sandboxes = [ "test/data/cabal-project"
                  , "test/data/check-packageid"
                  , "test/data/duplicate-pkgver/"
                  , "test/data/broken-cabal/"
                  ]
      genSandboxCfg dir = withDirectory dir $ \cwdir -> do
         system ("sed 's|@CWD@|" ++ cwdir ++ "|g' cabal.sandbox.config.in > cabal.sandbox.config")
      pkgDirs =
        [ "test/data/cabal-project/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d"
        , "test/data/check-packageid/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d"
        , "test/data/duplicate-pkgver/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d"]
      genGhcPkgCache dir = system $ "ghc-pkg recache --force -f" ++ dir

  genSandboxCfg `mapM_` sandboxes
  genGhcPkgCache `mapM_` pkgDirs

  let caches = [ "setup-config"
               , "setup-config.ghc-mod.cabal-helper"
               , "setup-config.ghc-mod.resolved-components"
               , "ghc-mod.cache"
               ]
      cachesFindExp :: String
      cachesFindExp = unwords $ intersperse "-o " $ map ("-name "++) caches

      cleanCmd = "find test \\( "++ cachesFindExp ++" \\) -exec rm {} \\;"

  putStrLn $ "$ " ++ cleanCmd
  void $ system cleanCmd
  void $ system "cabal --version"
  void $ system "ghc --version"

  (putStrLn =<< runD debugInfo)
      `E.catch` (\(_ :: E.SomeException) -> return () )

  hspec spec
