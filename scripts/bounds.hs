{-# LANGUAGE LambdaCase #-}
import Control.Arrow
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Function
import Data.Map (Map)
import qualified Data.Map as M
import System.Environment
import System.FilePath
import System.Directory

import Distribution.Verbosity
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription
import Distribution.Package
import Distribution.Version
import Distribution.Text
import Text.PrettyPrint
import System.Environment


datadir = "hackage-metadata"

main = do
  [pkg,v] <- getArgs

  let pkg_id = pkg ++ "-" ++ v
      cabal_file = datadir </> pkg_id <.> "cabal"

  pkg_time <- getTime pkg v

  ps <-
      mapMaybe (\case (p, "-any") -> Just p
                      _ -> Nothing)

                  <$> getDeps cabal_file
  vs <- mapM (getClosestVersion pkg_time) ps
  print $ ps `zip` vs
  return ()

check :: VersionRange -> Version -> Bool
check vr v = withinRange v vr

getClosestVersion :: Integer -> String -> IO String
getClosestVersion pkg_time dep = do
  vs <- getVersions dep
  ts <- mapM (getTime dep) vs
  let vtalist = sortBy (flip compare `on` snd) $ vs `zip` ts
      ((v,_t):_) = filter ((<pkg_time) . snd) vtalist
--      Just v' = simpleParse v
  return v

getVersions :: String -> IO [String]
getVersions p = do
  fs <- listDirectory datadir
  return $ nub
         $ map snd
         $ filter ((==p) . fst)
         $ map (parsePkgId . dropExtension)
         $ filter (p `isPrefixOf`) fs

getTime :: String -> String -> IO Integer
getTime p v = do
  let pkg_id = p ++ "-" ++ v
      file = datadir </> pkg_id <.> "upload-date"
  read <$> readFile file

getDeps f = do
  pd <- flattenPackageDescription <$> readPackageDescription silent f
  return $ --nubBy ((==) `on` fst) $
             [ (unPackageName n, (render . disp) v)
             | (Dependency n v) <- buildDepends pd
             , not $ unPackageName n `elem` [
                    "ghc-mod",
                    "ghc",
                    "array",
                    "base",
                    "bin-package-db",
                    "binary",
                    "bytestring",
                    "containers",
                    "deepseq",
                    "directory",
                    "filepath",
                    "ghc-binary",
                    "ghc-boot",
                    "ghc-boot-th",
                    "ghc-prim",
                    "ghci",
                    "haskelline",
                    "haskell2010",
                    "haskell98",
                    "haskell98",
                    "hoopl",
                    "hpc",
                    "integer-gmp",
                    "old-locale",
                    "old-time",
                    "pretty",
                    "process",
                    "random",
                    "rts",
                    "template-haskell",
                    "terminfo",
                    "time",
                    "transformers",
                    "unix",
                    "xhtml"
                   ]
             ]

parsePkgId pkg_id = let
    v:pkgcs = reverse $ splitOn "-" pkg_id
  in
    (intercalate "-" $ reverse pkgcs, v)
