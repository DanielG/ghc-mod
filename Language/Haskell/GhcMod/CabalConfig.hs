{-# LANGUAGE CPP #-}

-- | This module abstracts extracting information from Cabal's on-disk
-- 'LocalBuildInfo' (@dist/setup-config@) for different version combinations of
-- Cabal and GHC.
module Language.Haskell.GhcMod.CabalConfig (
    cabalConfigDependencies
  , cabalConfigFlags
  ) where

import Control.Applicative
import Distribution.Package (PackageIdentifier)
import Distribution.PackageDescription (FlagAssignment)

import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Error

#if __GLASGOW_HASKELL__ >= 710
import Language.Haskell.GhcMod.CabalConfig.Ghc710
#else
import Language.Haskell.GhcMod.CabalConfig.PreGhc710
#endif


-- | Get list of 'Package's needed by all components of the current package
cabalConfigDependencies :: (IOish m, MonadError GhcModError m)
                        => Cradle
                        -> PackageIdentifier
                        -> m [Package]
cabalConfigDependencies cradle thisPkg =
    configDependencies thisPkg <$> getConfig cradle


-- | Get the flag assignment from the local build info of the given cradle
cabalConfigFlags :: (IOish m, MonadError GhcModError m)
                 => Cradle
                 -> m FlagAssignment
cabalConfigFlags cradle = do
  config <- getConfig cradle
  case configFlags config of
    Right x  -> return x
    Left msg -> throwError (GMECabalFlags (GMEString msg))
