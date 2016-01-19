{-# LANGUAGE TupleSections, FlexibleInstances, Rank2Types, ImpredicativeTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Haskell.GhcMod.SrcUtils where

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
import qualified Language.Haskell.Exts.Annotated as HE
import Language.Haskell.GhcMod.Doc
import Language.Haskell.GhcMod.Gap
import qualified Language.Haskell.GhcMod.Gap as Gap
import OccName (OccName)
import Outputable (PprStyle)
import TcHsSyn (hsPatType)
import Prelude
import Control.Monad
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

type CstGenQS = M.Map Var Type
type CstGenQT a = forall m. GhcMonad m => a Id -> CstGenQS -> (m [(SrcSpan, Type)], CstGenQS)

collectSpansTypes :: (GhcMonad m) => Bool -> G.TypecheckedModule -> (Int, Int) -> m [(SrcSpan, Type)]
collectSpansTypes withConstraints tcs lc =
  everythingStagedWithContext TypeChecker M.empty (liftM2 (++))
    (return [])
    ((return [],)
      `mkQ`  (hsBind    :: CstGenQT G.LHsBind)
      `extQ` (genericCT :: CstGenQT G.LHsExpr)
      `extQ` (genericCT :: CstGenQT G.LPat)
      )
    (G.tm_typechecked_source tcs)
  where
    insExp x = M.insert (G.abe_mono x) (G.varType $ G.abe_poly x)
    hsBind (L _ G.AbsBinds{abs_exports = es'}) s
      | withConstraints = (return [], foldr insExp s es')
      | otherwise       = (return [], s)
    hsBind x s = genericCT x s
    genericCT x s = constrainedType' (collectBinders x) s x
    collectBinders :: Data a => a -> [Id]
    collectBinders = listifyStaged TypeChecker (const True)
    getType' x@(L spn _)
      | G.isGoodSrcSpan spn && spn `G.spans` lc
      = getType tcs x
      | otherwise = return Nothing
    constrainedType' pids s x
        | withConstraints
        = (maybe [] (uncurry $ constrainedType pids s) <$> getType' x, s)
        | otherwise = (maybeToList <$> getType' x, s)
    constrainedType pids s spn genTyp =
      let
        ctys  = mapMaybe build pids
        build x | Just cti <- x `M.lookup` s
                = let
                    (preds', ctt) = getPreds cti
                    vts = listifyStaged TypeChecker G.isTyVar $ G.varType x
                    tvm = listifyStaged TypeChecker G.isTyVarTy ctt
                  in Just (preds', zip vts tvm)
                | otherwise = Nothing
        preds = concatMap fst ctys
        subs  = G.mkTopTvSubst $ concatMap snd ctys
        ty    = G.substTy subs $ G.mkFunTys preds genTyp
      in [(spn, ty)]
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
typeSigInRangeHE lineNo colNo (HE.TypeFamDecl (HE.SrcSpanInfo s _) _ _) =
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
