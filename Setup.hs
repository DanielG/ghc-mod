import Distribution.Simple
import Distribution.Simple.LocalBuildInfo

import Control.Monad
import Control.Applicative
import Data.Version
import System.Process
import System.Exit
import Text.ParserCombinators.ReadP

-- import Data.Monoid
-- import Distribution.Simple.Setup
-- import Distribution.Simple.InstallDirs
-- main = defaultMainWithHooks $ simpleUserHooks {
--     confHook = \desc cf -> do
--         print desc
--         print cf
--         (confHook simpleUserHooks) desc cf {
--             configProgSuffix =
--                 configProgSuffix cf `mappend` toFlag (toPathTemplate "$compiler")
--         }
--  }

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
    postConf = \args cf desc lbi -> do
        -- I hope they never change this ;)
        ["cabal-install", "version", _cabalInstallVer, "using", "version", cabalVer', "of", "the", "Cabal", "library"] <- words <$> readProcess "cabal" ["--version"] ""

        let
          ghcVer = compilerVersion (compiler lbi)
          cabalVer = parseVer cabalVer'

          -- ghc >= 7.10?
          minGhc710 = ghcVer `withinRange` orLaterVersion (parseVer "7.10")

          [libCabalVer] = [ ver | (_, PackageIdentifier pkg ver)
                                    <- externalPackageDeps lbi
                          , pkg == PackageName "Cabal" ]

        if minGhc710 then
            -- make sure Cabal versions are consistent
            when (not $ cabalVer `sameMajorVersionAs` libCabalVer) $ do
                 putStrLn $ "Error: Cabal seems to have decided ghc-mod should be built using Cabal version "++showVersion libCabalVer++ " while the `cabal' executable in your PATH was built with Cabal version "++showVersion cabalVer++ ". This will lead to conflicts when running ghc-mod in any project where you use this `cabal' executable. Please compile ghc-mod using the same Cabal version as your `cabal' executable or recompile cabal-install using this version of the Cabal library. (See https://github.com/kazu-yamamoto/ghc-mod/wiki/InconsistentCabalVersions )"
                 exitFailure

        else -- ghc < 7.10
            -- make sure Cabal version is < 1.22
            when (not $ cabalVer `earlierVersionThan` (parseVer "1.22")) $ do
                 putStrLn "Error: when ghc-mod is built with GHC version < 7.10 only Cabal < 1.22 is supported. (See https://github.com/kazu-yamamoto/ghc-mod/wiki/InconsistentCabalVersions )"
                 exitFailure

        (postConf simpleUserHooks) args cf desc lbi
   }
 where
   parseVer str =
       case filter ((=="") . snd) $ readP_to_S parseVersion str of
         [(ver, _)] -> ver
         _ -> error $ "No parse (Ver) :(\n" ++ str ++ "\n"

   earlierVersionThan ver ver' =
       ver `withinRange` earlierVersion ver'
   sameMajorVersionAs ver ver' =
       ver `withinRange` withinVersion (Version (take 2 $ versionBranch ver') [])
