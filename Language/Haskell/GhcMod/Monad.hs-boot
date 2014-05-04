{-# LANGUAGE RoleAnnotations #-}
module Language.Haskell.GhcMod.Monad where

import DynFlags (HasDynFlags)
import Control.Monad.IO.Class (MonadIO)
import Control.Applicative (Applicative)

data GhcMod a
type role GhcMod nominal

instance Functor GhcMod
instance Applicative GhcMod
instance Monad GhcMod

instance HasDynFlags GhcMod
instance MonadIO GhcMod
