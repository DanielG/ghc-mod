-- TODO: remove CPP once Gap(ed)
{-# LANGUAGE CPP, TupleSections, FlexibleInstances, Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GhcMod.SrcUtils where

import Control.Applicative
import CoreUtils (exprType)
import Data.Generics
import Data.Maybe
import Data.Ord as O
import GHC (LHsExpr, LPat, Id, DynFlags, SrcSpan, Type, Located, ParsedSource, RenamedSource, TypecheckedSource, GenLocated(L))
import Var (Var)
import qualified GHC as G
import qualified Var as G
import qualified Type as G
import GHC.SYB.Utils
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

instance HasType (LHsExpr Id) where
    getType tcm e = do
        hs_env <- G.getSession
        mbe <- liftIO $ Gap.deSugar tcm e hs_env
        return $ (G.getLoc e, ) <$> CoreUtils.exprType <$> mbe

instance HasType (LPat Id) where
    getType _ (G.L spn pat) = return $ Just (spn, hsPatType pat)

----------------------------------------------------------------

-- | Stores mapping from monomorphic to polymorphic types
type CstGenQS = M.Map Var Type
-- | Generic type to simplify SYB definition
type CstGenQT a = forall m. GhcMonad m => a Id -> CstGenQS -> (m [(SrcSpan, Type)], CstGenQS)

collectSpansTypes :: (GhcMonad m) => Bool -> G.TypecheckedModule -> (Int, Int) -> m [(SrcSpan, Type)]
collectSpansTypes withConstraints tcs lc =
  -- This walks AST top-down, left-to-right, while carrying CstGenQS down the tree
  -- (but not left-to-right)
  everythingStagedWithContext TypeChecker M.empty (liftM2 (++))
    (return [])
    ((return [],)
      `mkQ`  (hsBind    :: CstGenQT G.LHsBind) -- matches on binds
      `extQ` (genericCT :: CstGenQT G.LHsExpr) -- matches on expressions
      `extQ` (genericCT :: CstGenQT G.LPat) -- matches on patterns
      )
    (G.tm_typechecked_source tcs)
  where
    -- Helper function to insert mapping into CstGenQS
    insExp x = M.insert (G.abe_mono x) (G.varType $ G.abe_poly x)
    -- If there is AbsBinds here, insert mapping into CstGenQS if needed
    hsBind (L _ G.AbsBinds{abs_exports = es'}) s
      | withConstraints = (return [], foldr insExp s es')
      | otherwise       = (return [], s)
#if __GLASGOW_HASKELL__ >= 800
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
    genericCT x s
      | withConstraints
      = (maybe [] (uncurry $ constrainedType (collectBinders x) s) <$> getType' x, s)
      | otherwise = (maybeToList <$> getType' x, s)
    -- Collects everything with Id from LHsBind, LHsExpr, or LPat
    collectBinders :: Data a => a -> [Id]
    collectBinders = listifyStaged TypeChecker (const True)
    -- Gets monomorphic type with location
    getType' x@(L spn _)
      | G.isGoodSrcSpan spn && spn `G.spans` lc
      = getType tcs x
      | otherwise = return Nothing
    -- Gets constrained type
    constrainedType :: [Var] -- ^ Binders in expression, i.e. anything with Id
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
                    -- list of type variables in monomorphic type
                    vts = listifyStaged TypeChecker G.isTyVar $ G.varType x
                    -- list of type variables in polymorphic type
                    tvm = listifyStaged TypeChecker G.isTyVarTy ctt
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
listifySpans tcs lc = listifyStaged TypeChecker p tcs
  where
    p (L spn _) = G.isGoodSrcSpan spn && spn `G.spans` lc

listifyParsedSpans :: Typeable a => ParsedSource -> (Int, Int) -> [Located a]
listifyParsedSpans pcs lc = listifyStaged Parser p pcs
  where
    p (L spn _) = G.isGoodSrcSpan spn && spn `G.spans` lc

listifyRenamedSpans :: Typeable a => RenamedSource -> (Int, Int) -> [Located a]
listifyRenamedSpans pcs lc = listifyStaged Renamer p pcs
  where
    p (L spn _) = G.isGoodSrcSpan spn && spn `G.spans` lc

listifyStaged :: Typeable r => Stage -> (r -> Bool) -> GenericQ [r]
listifyStaged s p = everythingStaged s (++) [] ([] `mkQ` (\x -> [x | p x]))

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
