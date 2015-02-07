{-# LANGUAGE CPP #-}

-- | This module abstracts extracting information from Cabal's on-disk
-- 'LocalBuildInfo' (@dist/setup-config@) for different version combinations of
-- Cabal and GHC.
module Language.Haskell.GhcMod.CabalConfig (
    CabalConfig
  , cabalGetConfig
  , cabalConfigDependencies
  , cabalConfigFlags
  ) where

import Distribution.Package (PackageIdentifier)
import Distribution.PackageDescription (FlagAssignment)

import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Error

import Language.Haskell.GhcMod.CabalConfig.Extract

cabalGetConfig :: (IOish m, GmError m) => Cradle -> m CabalConfig
cabalGetConfig = getConfig

-- | Get list of 'Package's needed by all components of the current package
cabalConfigDependencies :: CabalConfig -> PackageIdentifier -> [Package]
cabalConfigDependencies config thisPkg =
    configDependencies thisPkg config


-- | Get the flag assignment from the local build info of the given cradle
cabalConfigFlags :: (IOish m, GmError m) => CabalConfig -> m FlagAssignment
cabalConfigFlags config = do
  case configFlags config of
    Right x  -> return x
    Left msg -> throwError (GMECabalFlags (GMEString msg))
