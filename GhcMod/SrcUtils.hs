-- TODO: remove CPP once Gap(ed)
{-# LANGUAGE CPP, TupleSections, FlexibleInstances, Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GhcMod.SrcUtils where

import Control.Applicative
import CoreUtils (exprType)
import Data.Generics
import Data.Maybe
import Data.Ord as O
import GHC (LHsExpr, LPat, DynFlags, SrcSpan, Type, Located, ParsedSource, RenamedSource, TypecheckedSource, GenLocated(L))
import qualified GHC as G
import qualified Var as G
import qualified Type as G
#if __GLASGOW_HASKELL__ < 804
import GHC.SYB.Utils
#endif
import GhcMonad
import qualified Language.Haskell.Exts as HE
import GhcMod.Doc
import GhcMod.Gap
import qualified GhcMod.Gap as Gap
import OccName (OccName)
import Outputable (PprStyle)
import TcHsSyn (hsPatType)
import Prelude
import Control.Monad
import Data.List (nub)
import Control.Arrow
import qualified Data.Map as M

----------------------------------------------------------------

instance HasType (LHsExpr GhcTc) where
    getType tcm e = do
        hs_env <- G.getSession
        mbe <- liftIO $ Gap.deSugar tcm e hs_env
        return $ (G.getLoc e, ) <$> CoreUtils.exprType <$> mbe

instance HasType (LPat GhcTc) where
    getType _ (G.L spn pat) = return $ Just (spn, hsPatType pat)

----------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 804
-- | Stores mapping from monomorphic to polymorphic types
type CstGenQS = M.Map (G.IdP GhcTc) Type
-- | Generic type to simplify SYB definition
type CstGenQT m a = a GhcTc -> CstGenQS -> (m [(SrcSpan, Type)], CstGenQS)
#else
-- | Stores mapping from monomorphic to polymorphic types
type CstGenQS = M.Map G.Var Type
-- | Generic type to simplify SYB definition
type CstGenQT a = forall m. GhcMonad m => a G.Id -> CstGenQS -> (m [(SrcSpan, Type)], CstGenQS)
#endif

collectSpansTypes :: (GhcMonad m) => Bool -> G.TypecheckedModule -> (Int,Int) -> m [(SrcSpan, Type)]
collectSpansTypes withConstraints tcs lc = collectSpansTypes' withConstraints tcs (`G.spans` lc)

collectAllSpansTypes :: (GhcMonad m) => Bool -> G.TypecheckedModule -> m [(SrcSpan, Type)]
collectAllSpansTypes withConstraints tcs = collectSpansTypes' withConstraints tcs (const True)

collectSpansTypes' :: forall m. (GhcMonad m) => Bool -> G.TypecheckedModule -> (SrcSpan -> Bool) -> m [(SrcSpan, Type)]
-- collectSpansTypes' :: forall m.(GhcMonad m) => Bool -> G.TypecheckedModule -> (Int, Int) -> m [(SrcSpan, Type)]
collectSpansTypes' withConstraints tcs f =
  -- This walks AST top-down, left-to-right, while carrying CstGenQS down the tree
  -- (but not left-to-right)
#if __GLASGOW_HASKELL__ >= 804
  everythingWithContext M.empty (liftM2 (++))
#else
  everythingStagedWithContext TypeChecker M.empty (liftM2 (++))
    (return [])
#endif
    ((return [],)
      `mkQ`  (hsBind    :: G.LHsBind GhcTc -> CstGenQS -> (m [(SrcSpan, Type)], CstGenQS)) -- matches on binds
      `extQ` (genericCT :: G.LHsExpr GhcTc -> CstGenQS -> (m [(SrcSpan, Type)], CstGenQS)) -- matches on expressions
      `extQ` (genericCT :: G.LPat    GhcTc -> CstGenQS -> (m [(SrcSpan, Type)], CstGenQS)) -- matches on patterns

      )
    (G.tm_typechecked_source tcs)
  where
    -- Helper function to insert mapping into CstGenQS
    insExp x = M.insert (G.abe_mono x) (G.varType $ G.abe_poly x)
    -- If there is AbsBinds here, insert mapping into CstGenQS if needed

    hsBind (L _ G.AbsBinds{abs_exports = es'}) s
      | withConstraints = (return [], foldr insExp s es')
      | otherwise       = (return [], s)
#if __GLASGOW_HASKELL__ >= 804
#elif __GLASGOW_HASKELL__ >= 800
    -- TODO: move to Gap
    -- Note: this deals with bindings with explicit type signature, e.g.
    --    double :: Num a => a -> a
    --    double x = 2*x
    hsBind (L _ G.AbsBindsSig{abs_sig_export = poly, abs_sig_bind = bind}) s
      | withConstraints =
          let new_s =
                case bind of
                  G.L _ G.FunBind{fun_id = i} -> M.insert (G.unLoc i) (G.varType poly) s
                  _ -> s
          in (return [], new_s)
      | otherwise       = (return [], s)
#endif
    -- Otherwise, it's the same as other cases
    hsBind x s = genericCT x s
    -- Generic SYB function to get type
    genericCT :: forall b . (Data (b GhcTc), HasType (Located (b GhcTc)))
              => Located (b GhcTc) -> CstGenQS -> (m [(SrcSpan, Type)], CstGenQS)
    genericCT x s
      | withConstraints
      = (maybe [] (uncurry $ constrainedType (collectBinders x) s) <$> getType' x, s)
      | otherwise = (maybeToList <$> getType' x, s)
#if __GLASGOW_HASKELL__ >= 804
    -- Collects everything with Id from LHsBind, LHsExpr, or LPat
    collectBinders :: Data a => a -> [G.IdP GhcTc]
    collectBinders = listify (const True)
#else
    -- Collects everything with Id from LHsBind, LHsExpr, or LPat
    collectBinders :: Data a => a -> [G.Id]
    collectBinders = listifyStaged TypeChecker (const True)
#endif
    -- Gets monomorphic type with location
    getType' :: forall t . (HasType (Located t)) => Located t -> m (Maybe (SrcSpan, Type))
    getType' x@(L spn _)
      | G.isGoodSrcSpan spn && f spn
      = getType tcs x
      | otherwise = return Nothing
    -- Gets constrained type
#if __GLASGOW_HASKELL__ >= 804
    constrainedType :: [G.IdP GhcTc] -- ^ Binders in expression, i.e. anything with Id
#else
    constrainedType :: [G.Var] -- ^ Binders in expression, i.e. anything with Id
#endif
                    -> CstGenQS -- ^ Map from Id to polymorphic type
                    -> SrcSpan -- ^ extent of expression, copied to result
                    -> Type -- ^ monomorphic type
                    -> [(SrcSpan, Type)] -- ^ result
    constrainedType pids s spn genTyp =
      let
        -- runs build on every binder.
        ctys  = mapMaybe build (nub pids)
        -- Computes constrained type for x. Returns (constraints, substitutions)
        -- Substitutions are needed because type variables don't match
        -- between polymorphic and monomorphic types.
        -- E.g. poly type might be `Monad m => m ()`, while monomorphic might be `f ()`
        build x | Just cti <- x `M.lookup` s
                = let
                    (preds', ctt) = getPreds cti
#if __GLASGOW_HASKELL__ >= 804
                    -- list of type variables in monomorphic type
                    vts = listify G.isTyVar $ G.varType x
                    -- list of type variables in polymorphic type
                    tvm = listify G.isTyVarTy ctt
#else
                    -- list of type variables in monomorphic type
                    vts = listifyStaged TypeChecker G.isTyVar $ G.varType x
                    -- list of type variables in polymorphic type
                    tvm = listifyStaged TypeChecker G.isTyVarTy ctt
#endif
                  in Just (preds', zip vts tvm)
                | otherwise = Nothing
        -- list of constraints
        preds = concatMap fst ctys
        -- Type variable substitutions
#if __GLASGOW_HASKELL__ >= 800
 -- TODO: move to Gap
        subs  = G.mkTvSubstPrs $ concatMap snd ctys
#else
        subs  = G.mkTopTvSubst $ concatMap snd ctys
#endif
        -- Constrained type
        ty    = G.substTy subs $ G.mkFunTys preds genTyp
      in [(spn, ty)]
    -- Splits a given type into list of constraints and simple type. Drops foralls.
    getPreds :: Type -> ([Type], Type)
    getPreds x | G.isForAllTy x = getPreds $ G.dropForAlls x
               | Just (c, t) <- G.splitFunTy_maybe x
               , G.isPredTy c = first (c:) $ getPreds t
               | otherwise = ([], x)

listifySpans :: Typeable a => TypecheckedSource -> (Int, Int) -> [Located a]
#if __GLASGOW_HASKELL__ >= 804
listifySpans tcs lc = listify p tcs
#else
listifySpans tcs lc = listifyStaged TypeChecker p tcs
#endif
  where
    p (L spn _) = G.isGoodSrcSpan spn && spn `G.spans` lc

listifyParsedSpans :: Typeable a => ParsedSource -> (Int, Int) -> [Located a]
#if __GLASGOW_HASKELL__ >= 804
listifyParsedSpans pcs lc = listify p pcs
#else
listifyParsedSpans pcs lc = listifyStaged Parser p pcs
#endif
  where
    p (L spn _) = G.isGoodSrcSpan spn && spn `G.spans` lc

listifyRenamedSpans :: Typeable a => RenamedSource -> (Int, Int) -> [Located a]
#if __GLASGOW_HASKELL__ >= 804
listifyRenamedSpans pcs lc = listify p pcs
#else
listifyRenamedSpans pcs lc = listifyStaged Renamer p pcs
#endif
  where
    p (L spn _) = G.isGoodSrcSpan spn && spn `G.spans` lc

#if __GLASGOW_HASKELL__ < 804
listifyStaged :: Typeable r => Stage -> (r -> Bool) -> GenericQ [r]
listifyStaged s p = everythingStaged s (++) [] ([] `mkQ` (\x -> [x | p x]))
#endif

cmp :: SrcSpan -> SrcSpan -> Ordering
cmp a b
  | a `G.isSubspanOf` b = O.LT
  | b `G.isSubspanOf` a = O.GT
  | otherwise           = O.EQ

toTup :: DynFlags -> PprStyle -> (SrcSpan, Type) -> ((Int,Int,Int,Int),String)
toTup dflag style (spn, typ) = (fourInts spn, pretty dflag style typ)

fourInts :: SrcSpan -> (Int,Int,Int,Int)
fourInts = fromMaybe (0,0,0,0) . Gap.getSrcSpan

fourIntsHE :: HE.SrcSpan -> (Int,Int,Int,Int)
fourIntsHE loc = ( HE.srcSpanStartLine loc, HE.srcSpanStartColumn loc
                 , HE.srcSpanEndLine loc, HE.srcSpanEndColumn loc)

-- Check whether (line,col) is inside a given SrcSpanInfo
typeSigInRangeHE :: Int -> Int -> HE.Decl HE.SrcSpanInfo -> Bool
typeSigInRangeHE lineNo colNo (HE.TypeSig (HE.SrcSpanInfo s _) _ _) =
  HE.srcSpanStart s <= (lineNo, colNo) && HE.srcSpanEnd s >= (lineNo, colNo)
typeSigInRangeHE lineNo colNo (HE.TypeFamDecl (HE.SrcSpanInfo s _) _ _ _) =
  HE.srcSpanStart s <= (lineNo, colNo) && HE.srcSpanEnd s >= (lineNo, colNo)
typeSigInRangeHE lineNo colNo (HE.DataFamDecl (HE.SrcSpanInfo s _) _ _ _) =
  HE.srcSpanStart s <= (lineNo, colNo) && HE.srcSpanEnd s >= (lineNo, colNo)
typeSigInRangeHE _  _ _= False

pretty :: DynFlags -> PprStyle -> Type -> String
pretty dflag style = showOneLine dflag style . Gap.typeForUser

showName :: DynFlags -> PprStyle -> G.Name -> String
showName dflag style name = showOneLine dflag style $ Gap.nameForUser name

showOccName :: DynFlags -> PprStyle -> OccName -> String
showOccName dflag style name = showOneLine dflag style $ Gap.occNameForUser name
