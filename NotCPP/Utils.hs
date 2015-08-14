{-# LANGUAGE TemplateHaskell #-}
module NotCPP.Utils where

import Control.Applicative ((<$>))
import Language.Haskell.TH

-- | Turns 'Nothing' into an expression representing 'Nothing', and
-- @'Just' x@ into an expression representing 'Just' applied to the
-- expression in @x@.
liftMaybe :: Maybe Exp -> Exp
liftMaybe = maybe (ConE 'Nothing) (AppE (ConE 'Just))

-- | A useful variant of 'reify' that returns 'Nothing' instead of
-- halting compilation when an error occurs (e.g. because the given
-- name was not in scope).
maybeReify :: Name -> Q (Maybe Info)
maybeReify = recoverMaybe . reify

-- | Turns a possibly-failing 'Q' action into one returning a 'Maybe'
-- value.
recoverMaybe :: Q a -> Q (Maybe a)
recoverMaybe q = recover (return Nothing) (Just <$> q)

-- | Returns @'Just' ('VarE' n)@ if the info relates to a value called
-- @n@, or 'Nothing' if it relates to a different sort of thing.
infoToExp :: Info -> Maybe Exp
infoToExp (VarI n _ _ _) = Just (VarE n)
infoToExp (DataConI n _ _ _) = Just (ConE n)
infoToExp _ = Nothing
