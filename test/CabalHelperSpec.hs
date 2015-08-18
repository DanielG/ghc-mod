module CabalHelperSpec where

import Control.Arrow
import Control.Applicative
import Distribution.Helper
import Language.Haskell.GhcMod.CabalHelper
import Language.Haskell.GhcMod.PathsAndFiles
import Language.Haskell.GhcMod.Error
import Test.Hspec
import System.Directory
import System.FilePath
import System.Process (readProcess, system)

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
   name s = reverse $ stripDash $ stripDash $ reverse s

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

        it "handles stack project" $ do
            let tdir = "test/data/stack-project"
            opts <- map gmcGhcOpts <$> runD' tdir getComponents
            let ghcOpts = head opts
                pkgs = pkgOptions ghcOpts
            pkgs `shouldBe` ["Cabal","base","new-template"]

        it "extracts build dependencies" $ do
            let tdir = "test/data/cabal-project"
            opts <- map gmcGhcOpts <$> runD' tdir getComponents
            let ghcOpts = head opts
                pkgs = pkgOptions ghcOpts
            pkgs `shouldBe` ["Cabal","base","template-haskell"]

        it "uses non default flags" $ do
            let tdir = "test/data/cabal-flags"
            _ <- withDirectory_ tdir $
                readProcess "cabal" ["configure", "-ftest-flag"] ""

            opts <- map gmcGhcOpts <$> runD' tdir getComponents
            let ghcOpts = head opts
                pkgs = pkgOptions ghcOpts
            pkgs `shouldBe` ["Cabal","base"]

    describe "getCustomPkgDbStack" $ do
        it "works" $ do
            let tdir = "test/data/custom-cradle"
            Just stack <- runD' tdir $ getCustomPkgDbStack
            stack `shouldBe` [ GlobalDb
                             , UserDb
                             , PackageDb "package-db-a"
                             , PackageDb "package-db-b"
                             , PackageDb "package-db-c"
                             ]

    describe "getPackageDbStack'" $ do
        it "fixes out of sync custom pkg-db stack" $ do
            withDirectory_ "test/data/custom-cradle" $ do
                _ <- system "cabal configure"
                (s, s') <- runD $ do
                    Just stack <- getCustomPkgDbStack
                    withCabal $ do
                        stack' <- getCabalPackageDbStack
                        return (stack, stack')
                s' `shouldBe` s
