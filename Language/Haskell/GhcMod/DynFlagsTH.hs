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

{-# LANGUAGE CPP, TemplateHaskell #-}
module Language.Haskell.GhcMod.DynFlagsTH where

import Language.Haskell.TH.Syntax
import Control.Applicative
import Data.Maybe
import Data.Generics.Aliases
import Data.Generics.Schemes
import DynFlags
import Prelude

deriveEqDynFlags :: Q [Dec] -> Q [Dec]
deriveEqDynFlags qds = do
#if __GLASGOW_HASKELL__ <= 710
  ~(TyConI (DataD [] _ [] [ctor] _ ))
#else
  ~(TyConI (DataD [] _ [] _ [ctor] _ ))
#endif
      <- reify ''DynFlags
  let ~(RecC _ fs) = ctor

  a <- newName "a"
  b <- newName "b"

  e <- AppE (VarE 'and) . ListE <$> sequence (catMaybes $ map (eq a b) fs)

  tysig@(SigD n _) :_ <- qds

  return $ [tysig, FunD n [Clause [VarP a, VarP b] (NormalB e) []]]

 where
   eq :: Name -> Name -> (Name, Strict, Type) -> Maybe (Q Exp)
   eq a b (fn@(Name (OccName fon) _), _, ft)
       | not (isUneqable || isIgnored) = Just expr
       | otherwise = Nothing
    where
       isUneqable = everything (||) (mkQ False hasUnEqable) ft

       hasUnEqable ArrowT = True
       hasUnEqable (ConT (Name (OccName on) _))
           | any (==on) ignoredTypeNames = True
           | any (==on) ignoredTypeOccNames = True
       hasUnEqable _ = False

       isIgnored = fon `elem` ignoredNames

       ignoredNames = [ "pkgDatabase" -- 7.8
#if __GLASGOW_HASKELL__ <= 706
                      , "ways" -- 'Ways' is not exported :/
#endif
                      ]
       ignoredTypeNames =
           [ "LogAction"
           , "LogFinaliser"
           , "LogOutput"
           , "OverridingBool"
           , "Scheme"
           , "PackageState"
           , "Hooks"
           , "FlushOut"
           , "FlushErr"
           , "Settings" -- I think these can't cange at runtime
           ]
       ignoredTypeOccNames = [ "OnOff" ]

       fa = AppE (VarE fn) (VarE a)
       fb = AppE (VarE fn) (VarE b)
       expr =
         case fon of
           "rtsOptsEnabled" -> do
               eqfn <- [| let eqfn RtsOptsNone RtsOptsNone = True
                              eqfn RtsOptsSafeOnly RtsOptsSafeOnly = True
                              eqfn RtsOptsAll RtsOptsAll = True
                              eqfn _ _ = False
                          in eqfn
                       |]
               return $ AppE (AppE eqfn fa) fb

#if __GLASGOW_HASKELL__ >= 710 && __GLASGOW_HASKELL__ < 800
           "sigOf" -> do
               eqfn <- [| let eqfn NotSigOf NotSigOf = True
                              eqfn (SigOf a') (SigOf b') = a' == b'
                              eqfn (SigOfMap a') (SigOfMap b') = a' == b'
                              eqfn _ _ = False
                          in eqfn
                       |]
               return $ AppE (AppE eqfn fa) fb
#endif

#if __GLASGOW_HASKELL <= 706
           "profAuto" -> do
               eqfn <- [| let eqfn NoProfAuto NoProfAuto = True
                              eqfn ProfAutoAll ProfAutoAll = True
                              eqfn ProfAutoTop ProfAutoTop = True
                              eqfn ProfAutoExports ProfAutoExports = True
                              eqfn ProfAutoCalls ProfAutoCalls = True
                              eqfn _ _ = False
                          in eqfn
                       |]
               return $ AppE (AppE eqfn fa) fb
#endif

#if __GLASGOW_HASKELL__ >= 706
           "language" -> do
               eqfn <- [| let eqfn (Just Haskell98) (Just Haskell98) = True
                              eqfn (Just Haskell2010) (Just Haskell2010) = True
                              eqfn _ _ = False
                          in eqfn
                       |]
               return $ AppE (AppE eqfn fa) fb
#endif

           _ ->
               return $ InfixE (Just fa) (VarE '(==)) (Just fb)
