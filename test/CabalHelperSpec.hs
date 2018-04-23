{-# LANGUAGE CPP #-}
module CabalHelperSpec where

import Control.Arrow
import Control.Applicative
import Distribution.Helper hiding ( (<.>) )
import GhcMod.CabalHelper
import GhcMod.PathsAndFiles
import GhcMod.Error
import Test.Hspec
import System.Directory
import System.FilePath
import System.Process
import Prelude

import Dir
import TestUtils
import Data.List

import Config (cProjectVersionInt)

ghcVersion :: Int
ghcVersion = read cProjectVersionInt

gmeProcessException :: GhcModError -> Bool
gmeProcessException GMEProcess {} = True
gmeProcessException _ = False

pkgOptions :: [String] -> [String]
pkgOptions [] = []
pkgOptions (_:[]) = []
pkgOptions (x:y:xs) | x == "-package-id" = [name y] ++ pkgOptions xs
                    | otherwise = pkgOptions (y:xs)
 where
   stripDash s = maybe s id $ (flip drop s . (+1) <$> findIndex (=='-') s)
#if __GLASGOW_HASKELL__ >= 800
   name s = reverse $ stripDash $ reverse s
#else
   name s = reverse $ stripDash $ stripDash $ reverse s
#endif

idirOpts :: [(c, [String])] -> [(c, [String])]
idirOpts = map (second $ map (drop 2) . filter ("-i"`isPrefixOf`))

spec :: Spec
spec = do
    describe "getComponents" $ do
        it "throws an exception if the cabal file is broken" $ do
            let tdir = "test/data/broken-cabal"
            runD' tdir getComponents `shouldThrow` anyIOException

        it "handles sandboxes correctly" $ do
            let tdir = "test/data/cabal-project"
            cwd <- getCurrentDirectory

            -- TODO: ChSetupHsName should also have sandbox stuff, see related
            -- comment in cabal-helper
            opts <- map gmcGhcOpts . filter ((/= ChSetupHsName) . gmcName) <$> runD' tdir getComponents

            bp <- buildPlatform readProcess
            if ghcVersion < 706
              then forM_ opts (\o -> o `shouldContain` ["-no-user-package-conf","-package-conf", cwd </> "test/data/cabal-project/.cabal-sandbox/"++ghcSandboxPkgDbDir bp])
              else forM_ opts (\o -> o `shouldContain` ["-no-user-package-db","-package-db",cwd </> "test/data/cabal-project/.cabal-sandbox/"++ghcSandboxPkgDbDir bp])

#if !MIN_VERSION_ghc(7,8,0)
        it "handles stack project" $ do
            let tdir = "test/data/stack-project"
            [ghcOpts] <- map gmcGhcOpts . filter ((==ChExeName "new-template-exe") . gmcName) <$> runD' tdir getComponents
            let pkgs = pkgOptions ghcOpts
            sort pkgs `shouldBe` ["base", "bytestring"]
#endif

        it "extracts build dependencies" $ do
            let tdir = "test/data/cabal-project"
            opts <- map gmcGhcOpts <$> runD' tdir getComponents
            let ghcOpts:_ = opts
                pkgs = pkgOptions ghcOpts
            pkgs `shouldBe` ["Cabal","base","template-haskell"]

        it "uses non default flags and preserves them across reconfigures" $ do
            let tdir = "test/data/cabal-flags"
            _ <- withDirectory_ tdir $
                readProcess "cabal" ["configure", "-ftest-flag"] ""

            let test = do
                  opts <- map gmcGhcOpts <$> runD' tdir getComponents
                  let ghcOpts = head opts
                      pkgs = pkgOptions ghcOpts
                  pkgs `shouldBe` ["base","Cabal"]

            test

            touch $ tdir </> "cabal-flags.cabal"

            test

touch :: FilePath -> IO ()
touch fn = do
  f <- readFile fn
  writeFile (fn <.> "tmp") f
  renameFile (fn <.> "tmp") fn
