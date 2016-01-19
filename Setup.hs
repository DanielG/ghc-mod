#!/usr/bin/env runhaskell
{-# LANGUAGE RecordWildCards #-}
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Install
import Distribution.Simple.Register
import Distribution.Simple.InstallDirs as ID
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Data.Version
import Data.Monoid
import System.Process
import System.Exit
import System.FilePath
import Text.ParserCombinators.ReadP

import SetupCompat

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
   confHook = \(gpd, hbi) cf ->
              xBuildDependsLike <$> (confHook simpleUserHooks) (gpd, hbi) cf

 , instHook = inst
 , copyHook = copy

-- , postConf = sanityCheckCabalVersions
 }

xBuildDependsLike :: LocalBuildInfo -> LocalBuildInfo
xBuildDependsLike lbi =
  let
      cc = componentsConfigs lbi
      pd = localPkgDescr lbi
      deps = dependsMap lbi
  in setComponentsConfigs lbi
        [ (cn, updateClbi deps comp clbi, cdeps)
        | (cn, clbi, cdeps) <- cc
        , let comp = getComponent pd cn
        ]

 where
   updateClbi deps comp clbi = setUnionDeps (otherDeps deps comp) clbi

   dependsMap ::
    LocalBuildInfo -> [(ComponentName, Deps)]
   dependsMap lbi =
       second getDeps <$> allComponentsInBuildOrder lbi

   otherDeps :: [(ComponentName, Deps)] -> Component -> Deps
   otherDeps deps comp = fromMaybe noDeps $
       flip lookup deps =<< read <$> lookup "x-build-depends-like" fields
      where
        fields = customFieldsBI (componentBuildInfo comp)

-- mostly copypasta from 'defaultInstallHook'
inst ::
    PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags -> IO ()
inst pd lbi _uf ifl = do
  let copyFlags = defaultCopyFlags {
                      copyDistPref   = installDistPref ifl,
                      copyDest       = toFlag NoCopyDest,
                      copyVerbosity  = installVerbosity ifl
                  }
  xInstallTarget pd lbi (\pd' lbi' -> install pd' lbi' copyFlags)
  let registerFlags = defaultRegisterFlags {
                          regDistPref  = installDistPref ifl,
                          regInPlace   = installInPlace ifl,
                          regPackageDB = installPackageDB ifl,
                          regVerbosity = installVerbosity ifl
                      }
  when (hasLibs pd) $ register pd lbi registerFlags

copy :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO ()
copy pd lbi _uh cf =
    xInstallTarget pd lbi (\pd' lbi' -> install pd' lbi' cf)

xInstallTarget :: PackageDescription
               -> LocalBuildInfo
               -> (PackageDescription -> LocalBuildInfo -> IO ())
               -> IO ()
xInstallTarget pd lbi fn = do
  let (extended, regular) = partition (isJust . installTarget) (executables pd)

  let pd_regular = pd { executables = regular }

  _ <- flip mapM extended $ \exe -> do
    putStrLn $ "extended "  ++ show (exeName exe)

    let
        idirtpl          = installDirTemplates lbi
        env              = installDirsTemplateEnv idirtpl
        libexecdir'      = fromPathTemplate (libexecdir idirtpl)

        pd_extended      = onlyExePackageDesc [exe] pd
        install_target   = fromJustNote "xInstallTarget" $ installTarget exe
        install_target'  = ID.substPathTemplate env install_target
        -- $libexec isn't a real thing :/ so we have to simulate it
        install_target'' = substLibExec' libexecdir' install_target'

    let lbi' = lbi {
                 installDirTemplates =
                     (installDirTemplates lbi) {
                   bindir = install_target''
                 }
               }
    fn pd_extended lbi'

  fn pd_regular lbi

 where
   installTarget :: Executable -> Maybe PathTemplate
   installTarget exe =
    toPathTemplate <$> lookup "x-install-target" (customFieldsBI $ buildInfo exe)

   substLibExec libexecdir "$libexecdir" = libexecdir
   substLibExec _ comp = comp

   substLibExec' dir =
       withPT $
           withSP $ map (substLibExec dir . dropTrailingPathSeparator)


   withPT f pt = toPathTemplate $ f (fromPathTemplate pt)
   withSP f p  = joinPath $ f (splitPath p)

onlyExePackageDesc :: [Executable] -> PackageDescription -> PackageDescription
onlyExePackageDesc exes pd = emptyPackageDescription {
                     package = package pd
                   , executables = exes
                   }

parseVer str =
    case filter ((=="") . snd) $ readP_to_S parseVersion str of
      [(ver, _)] -> ver
      _ -> error $ "No parse (Ver) :(\n" ++ str ++ "\n"

fromJustNote :: String -> Maybe a -> a
fromJustNote _ (Just x) = x
fromJustNote note Nothing = error $ "fromJustNote ("++note++"): "

-- sanityCheckCabalVersions args cf desc lbi = do
--   (cabalInstallVer, cabalVer) <- getCabalExecVer

--   let
--         ghcVer = compilerVersion (compiler lbi)
--         -- ghc >= 7.10?
--         minGhc710 = ghcVer `withinRange` orLaterVersion (parseVer "7.10")

--   when minGhc710 $ do
--     let cabalHelperCabalVer = compCabalVer (CExeName "cabal-helper")

--     when (not $ cabalVer `sameMajorVersionAs` cabalHelperCabalVer) $
--          failCabalVersionDifferent cabalVer cabalHelperCabalVer

--   -- carry on as usual
--   (postConf simpleUserHooks) args cf desc lbi

--  where
--    earlierVersionThan ver ver' =
--        ver `withinRange` earlierVersion ver'
--    sameMajorVersionAs ver ver' =
--        ver `withinRange` withinVersion (Version (take 2 $ versionBranch ver') [])

--    compCabalVer comp = let
--        clbi = getComponentLocalBuildInfo lbi comp

--        [cabalVer] =
--            [ ver | (_, PackageIdentifier pkg ver) <- componentPackageDeps clbi
--            , pkg == PackageName "Cabal" ]
--      in cabalVer

-- getCabalExecVer = do
--   ["cabal-install", "version", cabalInstallVer, "using", "version", cabalVer, "of", "the", "Cabal", "library"] <- words <$> readProcess "cabal" ["--version"] ""
--   return (parseVer cabalInstallVer, parseVer cabalVer)

-- failCabalVersionDifferent cabalVer libCabalVer =
--   putStrLn rerr  >> exitFailure
--  where
--    replace :: String -> String -> String -> String
--    replace _ _ [] = []
--    replace n r h@(h':hs)
--        | map snd (n `zip` h ) == n = r ++ replace n r (drop (length n) h)
--        | otherwise = h':replace n r hs

--    rerr = replace "X.XX.X.X" (showVersion libCabalVer) $
--           replace "Y.YY.Y.Y" (showVersion cabalVer) err
--    err = "\
-- \Error: Cabal seems to have decided ghc-mod should be built using Cabal\n\
-- \X.XX.X.X while the `cabal' executable in your PATH was built with Cabal\n\
-- \Y.YY.Y.Y. This will lead to conflicts when running ghc-mod in any project\n\
-- \where you use this `cabal' executable. Please compile ghc-mod using the same\n\
-- \Cabal version as your `cabal' executable or recompile cabal-install using\n\
-- \this version of the Cabal library.\n\
-- \\n\
-- \See: https://github.com/kazu-yamamoto/ghc-mod/wiki/InconsistentCabalVersions\n"
