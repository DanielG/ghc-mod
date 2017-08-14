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

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE CPP #-}
-- Using CPP so you don't have to :)
module NotCPP.Declarations where

import Control.Arrow
import Control.Applicative
import Data.Maybe
import Language.Haskell.TH.Syntax

import NotCPP.LookupValueName

nT :: Monad m => String -> m Type
cT :: Monad m => String -> m Type
nE :: Monad m => String -> m Exp
nP :: Monad m => String -> m Pat

nT str = return $ VarT (mkName str)
cT str = return $ ConT (mkName str)
nE str = return $ VarE (mkName str)
nP str = return $ VarP (mkName str)
recUpdE' :: Q Exp -> Name -> Exp -> Q Exp
recUpdE' ex name assign = do
  RecUpdE <$> ex <*> pure [(name, assign)]

lookupName' :: (NameSpace, String) -> Q (Maybe Name)
lookupName' (VarName, n) = lookupValueName n
lookupName' (DataName, n) = lookupValueName n
lookupName' (TcClsName, n) = lookupTypeName n

-- Does this even make sense?
ifelseD :: Q [Dec] -> Q [Dec] -> Q [Dec]
ifelseD if_decls' else_decls = do
  if_decls <- if_decls'
  alreadyDefined <- definedNames (boundNames `concatMap` if_decls)
  case alreadyDefined of
    [] -> if_decls'
    _ -> else_decls

ifdefelseD, ifelsedefD :: String -> Q [Dec] -> Q [Dec] -> Q [Dec]
ifelsedefD = ifdefelseD
ifdefelseD ident if_decls else_decls = do
  exists <- isJust <$> lookupValueName ident
  if exists
    then if_decls
    else else_decls

ifdefD :: String -> Q [Dec] -> Q [Dec]
ifdefD ident decls  = ifdefelseD ident decls (return [])

ifndefD :: String -> Q [Dec] -> Q [Dec]
ifndefD ident decls  = ifdefelseD ident (return []) decls

-- | Each of the given declarations is only spliced if the identifier it defines
-- is not defined yet.
--
-- For example:
--
-- @$(ifD [[d| someFunctionThatShouldExist x = x+1 |]]@
--
-- If @someFunctionThatShouldExist@ doesn't actually exist the definition given
-- in the splice will be the result of the splice otherwise nothing will be
-- spliced.
--
-- Currently this only works for function declarations but it can be easily
-- extended to other kinds of declarations.
ifD :: Q [Dec] -> Q [Dec]
ifD decls' = do
  decls <- decls'
  concat <$> flip mapM decls (\decl -> do
    alreadyDefined <- definedNames (boundNames decl)
    case alreadyDefined of
      [] -> return [decl]
      _ -> return [])

definedNames :: [(NameSpace, Name)] -> Q [Name]
definedNames ns = catMaybes <$> (lookupName' . second nameBase) `mapM` ns

boundNames :: Dec -> [(NameSpace, Name)]
boundNames decl =
    case decl of
      SigD n _ -> [(VarName, n)]
      FunD n _cls -> [(VarName, n)]
#if __GLASGOW_HASKELL__ >= 706
      InfixD _ n -> [(VarName, n)]
#endif
      ValD p _ _ -> map ((,) VarName) $ patNames p

      TySynD n _ _ -> [(TcClsName, n)]
      ClassD _ n _ _ _ -> [(TcClsName, n)]

#if __GLASGOW_HASKELL__ >= 800
      DataD _ n _ _ ctors _ ->
#else
      DataD _ n _ ctors _ ->
#endif
          [(TcClsName, n)] ++ map ((,) TcClsName) (conNames `concatMap` ctors)

#if __GLASGOW_HASKELL__ >= 800
      NewtypeD _ n _ _ ctor _ ->
#else
      NewtypeD _ n _ ctor _ ->
#endif
          [(TcClsName, n)] ++ map ((,) TcClsName) (conNames ctor)

#if __GLASGOW_HASKELL__ >= 800
      DataInstD _ _n _ _ ctors _ ->
#else
      DataInstD _ _n _ ctors _ ->
#endif
          map ((,) TcClsName) (conNames `concatMap` ctors)

#if __GLASGOW_HASKELL__ >= 800
      NewtypeInstD _ _n _ _ ctor _ ->
#else
      NewtypeInstD _ _n _ ctor _ ->
#endif
          map ((,) TcClsName) (conNames ctor)

      InstanceD {}  -> -- _ _ty _
          error "notcpp: Instance declarations are not supported yet"
      ForeignD _ ->
          error "notcpp: Foreign declarations are not supported yet"
      PragmaD _pragma -> error "notcpp: pragmas are not supported yet"

#if __GLASGOW_HASKELL__ >= 708
      TySynInstD _n _ -> error "notcpp: TySynInstD not supported yet"
#else
      TySynInstD _n _ _ -> error "notcpp: TySynInstD not supported yet"
#endif

#if __GLASGOW_HASKELL__ >= 708
      RoleAnnotD _n _ -> error "notcpp: RoleAnnotD not supported yet"
#endif

#if __GLASGOW_HASKELL__ >= 704 && __GLASGOW_HASKELL__ < 800
      FamilyD _ n _ _ -> [(TcClsName, n)]
#elif __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 800
      ClosedTypeFamilyD n _ _ _ -> [(TcClsName, n)]
#else
      OpenTypeFamilyD (TypeFamilyHead n _ _ _) -> [(TcClsName, n)]
      ClosedTypeFamilyD (TypeFamilyHead n _ _ _) _ -> [(TcClsName, n)]

#endif

conNames :: Con -> [Name]
conNames con =
    case con of
      NormalC n _ -> [n]
      RecC n _ -> [n]
      InfixC _ n _ -> [n]
      ForallC _ _ c -> conNames c

patNames :: Pat -> [Name]
patNames p'' =
    case p'' of
      LitP _         -> []
      VarP n         -> [n]
      TupP ps        -> patNames `concatMap` ps
      UnboxedTupP ps -> patNames `concatMap` ps
      ConP _ ps      -> patNames `concatMap` ps
      InfixP p _ p'  -> patNames `concatMap` [p,p']
      UInfixP p _ p' -> patNames `concatMap` [p,p']
      ParensP p      -> patNames p
      TildeP p       -> patNames p
      BangP p        -> patNames p
      AsP n p        -> n:(patNames p)
      WildP          -> []
      RecP _ fps     -> patNames `concatMap` map snd fps
      ListP ps       -> patNames `concatMap` ps
      SigP p _       -> patNames p
      ViewP _ p      -> patNames p
