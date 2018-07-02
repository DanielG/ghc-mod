-- ghc-mod: Happy Haskell Hacking
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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module GhcMod.DynFlagsTH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Applicative
#if __GLASGOW_HASKELL__ >= 804
import GHC.LanguageExtensions
import qualified EnumSet as E
import qualified Data.Set as IS
#else
import qualified Data.IntSet as IS
#endif
import Data.Maybe
import Data.Generics.Aliases
import Data.Generics.Schemes
import DynFlags
import Prelude

-- -------------------------------------

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

  e <- ListE <$> sequence (mapMaybe (eq a b) fs)

  tysig@(SigD n _) :_ <- qds

  return $ [tysig, FunD n [Clause [VarP a, VarP b] (NormalB e) []]]

 where
   eq :: Name -> Name -> (Name, Strict, Type) -> Maybe (Q Exp)
   eq a b (fun@(Name (OccName fon) _), _, ft)
       | not (isUneqable || isIgnored) = Just expr
       | otherwise = Nothing
    where
       isUneqable = everything (||) (mkQ False hasUnEqable) ft

       hasUnEqable (AppT (ConT (Name (OccName on) _)) _)
           | any (==on) ignoredConstructorNames = True
       hasUnEqable (ConT (Name (OccName on) _))
           | any (==on) ignoredTypeNames = True
           | any (==on) ignoredTypeOccNames = True
       hasUnEqable _ = False

       isIgnored = fon `elem` ignoredNames

       ignoredConstructorNames = [ "IORef" ]

       ignoredNames = [ "pkgDatabase" -- 7.8
#if __GLASGOW_HASKELL__ <= 706
                      , "ways" -- 'Ways' is not exported :/
#endif
                      ]
       ignoredTypeNames =
           [ "LogAction"
           , "LogFinaliser"
           , "PackageState"
           , "IO"
           , "Hooks"
           , "FlushOut"
           , "FlushErr"
           , "Settings" -- I think these can't cange at runtime
           , "LogFinaliser"   -- added for ghc-8.2
           , "LogOutput"      -- added for ghc-8.2
           , "OverridingBool" -- added for ghc-8.2
           , "Scheme"         -- added for ghc-8.2
           , "LoadedPlugin"   -- added for ghc-8.6
           ]
       ignoredTypeOccNames = [ "OnOff" ]

       fa = AppE (VarE fun) (VarE a)
       fb = AppE (VarE fun) (VarE b)
       expr =
         case fon of
           "rtsOptsEnabled" -> do
               let eqfn = [| let fn RtsOptsNone RtsOptsNone = [(True, "")]
                                 fn RtsOptsSafeOnly RtsOptsSafeOnly = [(True, "")]
                                 fn RtsOptsAll RtsOptsAll = [(True, "")]
                                 fn _ _ = [(False, "rtsOptsEnabled changed")]
                             in fn
                          |]
               [e| $(eqfn) $(return fa) $(return fb) |]

           "extraPkgConfs" -> do
               let eqfn = [| let fn a' b' = cond a' b'

                                 cond a' b' = zz ++ ll
                                   where
                                    zz :: [(Bool,String)]
                                    zz = (zipWith eqpr (a' []) (b' []))

                                    ll :: [(Bool,String)]
                                    ll = [(   length (a' []) == length (b' [])
                                         , if length (a' []) == length (b' []) then "" else "extraPkgConfs length mismatch")]

                                 eqpr GlobalPkgConf GlobalPkgConf = (True, "")
                                 eqpr UserPkgConf UserPkgConf = (True, "")
                                 eqpr (PkgConfFile pa) (PkgConfFile pb) = (pa == pb,if pa == pb then "" else "extraPkgConfs changed")
                                 eqpr _ _ = (False, "extraPkgConfs changed")
                             in fn
                          |]
               [e| $(eqfn) $(return fa) $(return fb) |]

#if __GLASGOW_HASKELL__ >= 710 && __GLASGOW_HASKELL__ < 800
           "sigOf" -> do
               let eqfn = [| let fn NotSigOf NotSigOf = [(True, "")]
                                 fn (SigOf a') (SigOf b') = [(a' == b', if a' == b' then "" else "sigOf changed")]
                                 fn (SigOfMap a') (SigOfMap b') = [(a' == b', if a' == b' then "" else "sigOfMap changed")]
                                 fn _ _ = [(False, "sigOf changed")]
                             in fn
                          |]
               [e| $(eqfn) $(return fa) $(return fb) |]
#endif

#if __GLASGOW_HASKELL <= 706
           "profAuto" -> do
               let eqfn = [| let fn NoProfAuto NoProfAuto = [(True, "")]
                                 fn ProfAutoAll ProfAutoAll = [(True, "")]
                                 fn ProfAutoTop ProfAutoTop = [(True, "")]
                                 fn ProfAutoExports ProfAutoExports = [(True, "")]
                                 fn ProfAutoCalls ProfAutoCalls = [(True, "")]
                                 fn _ _ = [(False, "profAuto changed")]
                             in fn
                          |]
               [e| $(eqfn) $(return fa) $(return fb) |]
#endif

#if __GLASGOW_HASKELL__ >= 706
           "language" -> do
               let eqfn = [| let fn (Just Haskell98) (Just Haskell98) = [(True, "")]
                                 fn (Just Haskell2010) (Just Haskell2010) = [(True, "")]
                                 fn Nothing Nothing = [(True, "")]
                                 fn _ _ = [(False, "language changed")]
                             in fn
                          |]
               [e| $(eqfn) $(return fa) $(return fb) |]
#endif

           "outputFile" -> do
               let eqfn = [| let fn (Just f1) (Just f2) = [(f1 == f2, if f1 == f2 then "" else "outputFile changed")]
                                 fn _ _ = [(True, "")] -- anything with a Nothing is fine.
                             in fn
                          |]
               [e| $(eqfn) $(return fa) $(return fb) |]

           "generalFlags" -> checkIntSet "generalFlags"

           "warningFlags" -> checkIntSet "warningFlags"
#if __GLASGOW_HASKELL__ >= 804
           "dumpFlags"    -> checkIntSet "dumpFlags"
           "fatalWarningFlags" -> checkIntSet "fatalWarningFlags"
           "extensionFlags" -> checkIntSet "extensionFlags"
#endif

           _ ->
               [e| [($(return fa) == $(return fb), if $(return fa) == $(return fb) then "" else ("default changed:" ++ fon)  )] |]

       checkIntSet fieldName = do
               let eqfn = [| let fn aa' bb' = r
                                   where
#if __GLASGOW_HASKELL__ >= 804
                                     aa = toSet aa'
                                     bb = toSet bb'
#else
                                     aa = aa'
                                     bb = bb'
#endif

                                     uni = IS.union aa bb
                                     dif = IS.intersection aa bb
                                     delta = IS.difference uni dif
                                     r = if delta == IS.empty
                                           then [(True, "")]
                                           else [(False, fieldName ++ ":delta=" ++ (show delta) )]
                             in fn
                          |]
               [e| $(eqfn) $(return fa) $(return fb) |]


#if __GLASGOW_HASKELL__ >= 806
deriving instance Eq IncludeSpecs
#endif
#if __GLASGOW_HASKELL__ >= 804
toSet es = IS.fromList $ E.toList es

deriving instance Ord GeneralFlag
deriving instance Ord DynFlags.WarningFlag
deriving instance Ord DynFlags.DumpFlag
deriving instance Ord DynFlags.LlvmTarget
deriving instance Ord Extension
deriving instance Eq LlvmTarget
#endif
