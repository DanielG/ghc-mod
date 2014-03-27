{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.GhcMod.GHCChoice where

import Control.Exception (IOException)
import CoreMonad (liftIO)
import qualified Exception as GE
import GHC (Ghc, GhcMonad)

----------------------------------------------------------------

-- | Try the left 'Ghc' action. If 'IOException' occurs, try
--   the right 'Ghc' action.
(||>) :: Ghc a -> Ghc a -> Ghc a
x ||> y = x `GE.gcatch` (\(_ :: IOException) -> y)

-- | Go to the next 'Ghc' monad by throwing 'AltGhcgoNext'.
goNext :: Ghc a
goNext = liftIO . GE.throwIO $ userError "goNext"

-- | Run any one 'Ghc' monad.
runAnyOne :: [Ghc a] -> Ghc a
runAnyOne = foldr (||>) goNext

----------------------------------------------------------------

-- | Try the left 'GhcMonad' action. If 'IOException' occurs, try
--   the right 'GhcMonad' action.
(|||>) :: GhcMonad m => m a -> m a -> m a
x |||> y = x `GE.gcatch` (\(_ :: IOException) -> y)
