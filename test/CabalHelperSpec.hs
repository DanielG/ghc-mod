module CabalHelperSpec where

import Control.Arrow
import Control.Applicative
import Language.Haskell.GhcMod.CabalHelper
import Language.Haskell.GhcMod.PathsAndFiles
import Language.Haskell.GhcMod.Error
import Test.Hspec
import System.Directory
import System.FilePath
import System.Process (readProcess)

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
    describe "getGhcOptions" $ do
        it "throws an exception if the cabal file is broken" $ do
            let tdir = "test/data/broken-caba"
            runD' tdir getGhcOptions `shouldThrow` anyIOException

        it "handles sandboxes correctly" $ do
            let tdir = "test/data/cabal-project"
            cwd <- getCurrentDirectory

            opts <- runD' tdir getGhcOptions

            if ghcVersion < 706
              then forM_ opts (\(_, o) -> o `shouldContain` ["-no-user-package-conf","-package-conf", cwd </> "test/data/cabal-project/.cabal-sandbox/"++ghcSandboxPkgDbDir])
              else forM_ opts (\(_, o) -> o `shouldContain` ["-no-user-package-db","-package-db",cwd </> "test/data/cabal-project/.cabal-sandbox/"++ghcSandboxPkgDbDir])

        it "extracts build dependencies" $ do
            let tdir = "test/data/cabal-project"
            opts <- runD' tdir getGhcOptions
            let ghcOpts = snd $ head opts
                pkgs = pkgOptions ghcOpts
            pkgs `shouldBe` ["Cabal","base","template-haskell"]

        it "uses non default flags" $ do
            let tdir = "test/data/cabal-flags"
            _ <- withDirectory_ tdir $
                readProcess "cabal" ["configure", "-ftest-flag"] ""

            opts <- runD' tdir getGhcOptions
            let ghcOpts = snd $ head opts
                pkgs = pkgOptions ghcOpts
            pkgs `shouldBe` ["Cabal","base"]
