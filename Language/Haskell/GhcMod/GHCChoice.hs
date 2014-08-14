{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.GhcMod.GHCChoice where

import Control.Exception (IOException)
import CoreMonad (liftIO)
import qualified Exception as GE
import GHC (GhcMonad)

----------------------------------------------------------------

-- | Try the left 'Ghc' action. If 'IOException' occurs, try
--   the right 'Ghc' action.
(||>) :: GhcMonad m => m a -> m a -> m a
x ||> y = x `GE.gcatch` (\(_ :: IOException) -> y)

-- | Go to the next 'Ghc' monad by throwing 'AltGhcgoNext'.
goNext :: GhcMonad m => m a
goNext = liftIO . GE.throwIO $ userError "goNext"

-- | Run any one 'Ghc' monad.
runAnyOne :: GhcMonad m => [m a] -> m a
runAnyOne = foldr (||>) goNext
