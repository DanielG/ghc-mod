{-# LANGUAGE CPP, ScopedTypeVariables #-}
import Spec
import Dir

import Control.Exception as E
import Control.Monad (void)
import GhcMod (debugInfo)
import System.Process
import System.Environment
import Test.Hspec
import TestUtils

main :: IO ()
main = do
#if __GLASGOW_HASKELL__ >= 708
  unsetEnv "GHC_PACKAGE_PATH"
#endif
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

  let cleanCmd = "git clean -dXf test/data/"
  putStrLn $ "$ " ++ cleanCmd
  void $ system cleanCmd
  void $ system "cabal --version"
  void $ system "ghc --version"

  genSandboxCfg `mapM_` sandboxes
  genGhcPkgCache `mapM_` pkgDirs

  let stackDir = "test/data/stack-project"
  void $ withDirectory_ stackDir $ do
    let ghcver = let gvn = show (__GLASGOW_HASKELL__ :: Int)
                     (major, minor') = splitAt (length gvn - 2) gvn
                     minor = case dropWhile (=='0') minor' of
                                  "" -> "0"
                                  x  -> x
                 in major ++ "." ++ minor
    void $ system $ "sed '$ a resolver: ghc-" ++ ghcver ++ "' stack.yaml.in > stack.yaml"
    void $ system "stack setup"
    void $ system "stack build"

  (putStrLn =<< runD debugInfo)
      `E.catch` (\(_ :: E.SomeException) -> return () )

  hspec spec
