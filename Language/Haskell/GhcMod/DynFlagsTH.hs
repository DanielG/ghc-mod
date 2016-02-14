-- ghc-mod: Making Haskell development *more* fun
-- Copyright (C) 2015  Daniel Gröber <dxld ÄT darkboxed DOT org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE TemplateHaskell #-}
module Language.Haskell.GhcMod.DynFlagsTH where

import Language.Haskell.TH.Syntax
import Data.Maybe
import Data.Generics.Aliases
import Data.Generics.Schemes
import Packages
import Hooks
import DynFlags

deriveEqDynFlags :: Q [Dec] -> Q [Dec]
deriveEqDynFlags qds = do
  ~(TyConI (DataD [] _ [] [ctor] _ )) <- reify ''DynFlags
  let ~(RecC _ fs) = ctor

  a <- newName "a"
  b <- newName "b"

  e <- AppE (VarE 'and) . ListE <$> sequence (catMaybes $ map (eq a b) fs)

  tysig@(SigD n _) :_ <- qds

  return $ [tysig, FunD n [Clause [VarP a, VarP b] (NormalB e) []]]

 where
   eq :: Name -> Name -> (Name, Strict, Type) -> Maybe (Q Exp)
   eq a b (fn@(Name (OccName fon) _), _, ft)
       | not isUneqable = Just expr
       | otherwise = Nothing
    where
       isUneqable = everything (||) (mkQ False hasUnEqable) ft

       hasUnEqable ArrowT = True
       hasUnEqable (ConT n@(Name (OccName on) _))
           | n == ''LogAction = True
           | any (==n) ignoredTypeNames = True
           | any (==on) ignoredTypeOccNames = True
       hasUnEqable _ = False

       ignoredTypeNames =
           [ ''PackageState
           , ''Hooks
           , ''FlushOut
           , ''FlushErr
           , ''Settings -- I think these can't cange at runtime
           ]
       ignoredTypeOccNames = [ "OnOff" ]

       fa = AppE (VarE fn) (VarE a)
       fb = AppE (VarE fn) (VarE b)
       expr =
         case fon of
           "language" -> do
               eqfn <- [| let eqfn (Just Haskell98) (Just Haskell98) = True
                              eqfn (Just Haskell2010) (Just Haskell2010) = True
                              eqfn _ _ = False
                          in eqfn
                       |]
               return $ AppE (AppE eqfn fa) fb
           "rtsOptsEnabled" -> do
               eqfn <- [| let eqfn RtsOptsNone RtsOptsNone = True
                              eqfn RtsOptsSafeOnly RtsOptsSafeOnly = True
                              eqfn RtsOptsAll RtsOptsAll = True
                              eqfn _ _ = False
                          in eqfn
                       |]
               return $ AppE (AppE eqfn fa) fb

           "sigOf" -> do
               eqfn <- [| let eqfn NotSigOf NotSigOf = True
                              eqfn (SigOf a') (SigOf b') = a' == b'
                              eqfn (SigOfMap a') (SigOfMap b') = a' == b'
                              eqfn _ _ = False
                          in eqfn
                       |]
               return $ AppE (AppE eqfn fa) fb


           _ ->
               return $ InfixE (Just fa) (VarE '(==)) (Just fb)
