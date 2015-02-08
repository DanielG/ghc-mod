{-# LANGUAGE EmptyDataDecls, TemplateHaskell #-}
-- | 
-- The orphan instance problem is well-known in Haskell. This module
-- by no means purports to solve the problem, but provides a workaround
-- that may be significantly less awful than the status quo in some
-- cases.
--
-- Say I think that the 'Name' type should have an 'IsString' instance.
-- But I don't control either the class or the type, so if I define the
-- instance, and then the template-haskell package defines one, my code
-- is going to break.
--
-- 'safeInstance' can help me to solve this problem:
--
-- > safeInstance ''IsString [t| Name |] [d|
-- >   fromString = mkName |]
--
-- This will declare an instance only if one doesn't already exist.
-- Now anyone importing your module is guaranteed to get an instance
-- one way or the other.
--
-- This module is still highly experimental. The example given above
-- does work, but anything involving type variables or complex method
-- bodies may be less fortunate. The names of the methods are mangled
-- a bit, so using recursion to define them may not work. Define the
-- method outside the code and then use a simple binding as above.
--
-- If you use this code (successfully or unsuccessfully!), go fetch
-- the maintainer address from the cabal file and let me know!
module NotCPP.OrphanEvasion (
  MultiParams,
  safeInstance,
  safeInstance',
 ) where

import Control.Applicative

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import NotCPP.ScopeLookup

-- | An empty type used only to signify a multiparameter typeclass in
-- 'safeInstance'.
data MultiParams a

-- | Given @(forall ts. Cxt => t)@, return @(Cxt, [t])@.
-- Given @(forall ts. Cxt => 'MultiParams' (t1, t2, t3))@, return
-- @(Cxt, [t1, t2, t3])@.
--
-- This is used in 'safeInstance' to allow types to be specified more
-- easily with TH typequotes.
fromTuple :: Type -> (Cxt, [Type])
fromTuple ty = unTuple <$> case ty of
  ForallT _ cxt ty' -> (cxt, ty')
  _ -> ([], ty)
 where
  unTuple :: Type -> [Type]
  unTuple (AppT (ConT n) ta)
    | n == ''MultiParams = case unrollAppT ta of
      (TupleT{}, ts) -> ts
      _ -> [ty]
  unTuple t = [t]

-- | A helper function to unwind type application. 
-- Given @TyCon t1 t2 t3@, returns @(TyCon, [t1,t2,t3])@
unrollAppT :: Type -> (Type, [Type])
unrollAppT = go []
 where
  go acc (AppT tc ta) = go (ta : acc) tc
  go acc ty = (ty, reverse acc)

-- | Left inverse to unrollAppT, equal to @'foldl' 'AppT'@
rollAppT :: Type -> [Type] -> Type
rollAppT = foldl AppT

-- | @'safeInstance'' className cxt types methods@ produces an instance
-- of the given class if and only if one doesn't already exist.
--
-- See 'safeInstance' for a simple way to construct the 'Cxt' and
-- @['Type']@ parameters.
safeInstance' :: Name -> Cxt -> [Type] -> Q [Dec] -> Q [Dec]
safeInstance' cl cxt tys inst = do
  b <- $(scopeLookups ["isInstance", "isClassInstance"]) cl tys
  if b
    then return []
    else do
      ds <- map fixInst <$> inst
      return [InstanceD cxt (rollAppT (ConT cl) tys) ds]
 where
  fixInst (FunD n cls) = FunD (fixName n) cls
  fixInst (ValD (VarP n) rhs wh) = ValD (VarP (fixName n)) rhs wh
  fixInst d = d
  fixName (Name n _) = Name n NameS

-- | 'safeInstance' is a more convenient version of 'safeInstance''
-- that takes the context and type from a @'Q' 'Type'@ with the intention
-- that it be supplied using a type-quote.
--
-- To define an instance @Show a => Show (Wrapper a)@, you'd use:
--
-- > safeInstance ''Show [t| Show a => Wrapper a |]
-- >   [d| show _ = "stuff" |]
--
-- To define an instance of a multi-param type class, use the
-- 'MultiParams' type constructor with a tuple:
--
-- > safeInstance ''MonadState
-- >   [t| MonadState s m => MultiParams (s, MaybeT m) |]
-- >   [d| put = ... |]
safeInstance :: Name -> Q Type -> Q [Dec] -> Q [Dec]
safeInstance n qty inst = do
  (cxt, tys) <- fromTuple <$> qty
  safeInstance' n cxt tys inst
