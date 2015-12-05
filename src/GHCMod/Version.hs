module GHCMod.Version where

import Paths_ghc_mod
import Data.Version (showVersion)
import Config (cProjectVersion)

progVersion :: String -> String
progVersion pf =
    "ghc-mod"++pf++" version " ++ showVersion version ++ " compiled by GHC "
                               ++ cProjectVersion

ghcModVersion :: String
ghcModVersion = progVersion ""

ghcModiVersion :: String
ghcModiVersion = progVersion "i"
