{-# LANGUAGE TemplateHaskell #-}
-- |
-- This module exports 'scopeLookup', which will find a variable or
-- value constructor for you and present it for your use. E.g. at some
-- point in the history of the acid-state package, 'openAcidState' was
-- renamed 'openLocalState'; for compatibility with both, you could
-- use:
--
-- > openState :: IO (AcidState st)
-- > openState = case $(scopeLookup "openLocalState") of
-- >   Just open -> open defaultState
-- >   Nothing -> case $(scopeLookup "openAcidState") of
-- >     Just open -> open defaultState
-- >     Nothing -> error
-- >       "openState: runtime name resolution has its drawbacks :/"
--
-- Or, for this specific case, you can use 'scopeLookups':
--
-- > openState :: IO (AcidState st)
-- > openState = open defaultState
-- >  where
-- >   open = $(scopeLookups ["openLocalState","openAcidState"])
--
-- Now if neither of the names are found then TH will throw a
-- compile-time error.
module NotCPP.ScopeLookup (
  scopeLookup,
  scopeLookups,
  scopeLookup',
  liftMaybe,
  recoverMaybe,
  maybeReify,
  infoToExp,
 ) where

import Control.Applicative ((<$>))

import Language.Haskell.TH (Q, Exp, recover, reify)

import NotCPP.LookupValueName
import NotCPP.Utils

-- | Produces a spliceable expression which expands to @'Just' val@ if
-- the given string refers to a value @val@ in scope, or 'Nothing'
-- otherwise.
--
-- @scopeLookup = 'fmap' 'liftMaybe' . 'scopeLookup''@
scopeLookup :: String -> Q Exp
scopeLookup = fmap liftMaybe . scopeLookup'

-- | Finds the first string in the list that names a value, and produces
-- a spliceable expression of that value, or reports a compile error if
-- it fails.
scopeLookups :: [String] -> Q Exp
scopeLookups xs = foldr
  (\s r -> maybe r return =<< scopeLookup' s)
  (fail ("scopeLookups: none found: " ++ show xs))
  xs

-- | Produces @'Just' x@ if the given string names the value @x@,
-- or 'Nothing' otherwise.
scopeLookup' :: String -> Q (Maybe Exp)
scopeLookup' s = recover (return Nothing) $ do
  Just n <- lookupValueName s
  infoToExp <$> reify n
