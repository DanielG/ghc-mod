{-# LANGUAGE TupleSections, FlexibleInstances, Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Haskell.GhcMod.SrcUtils where

import Control.Applicative ((<$>))
import CoreMonad (liftIO)
import CoreUtils (exprType)
import Data.Generics
import Data.Maybe (fromMaybe)
import Data.Ord as O
import GHC (Ghc, LHsExpr, LPat, Id, DynFlags, SrcSpan, Type, Located, ParsedSource, RenamedSource, TypecheckedSource, GenLocated(L))
import qualified GHC as G
import GHC.SYB.Utils (Stage(..), everythingStaged)
import Language.Haskell.GhcMod.Doc (showOneLine, getStyle)
import Language.Haskell.GhcMod.GHCApi
import Language.Haskell.GhcMod.Gap (HasType(..), setWarnTypedHoles, setDeferTypeErrors)
import qualified Language.Haskell.GhcMod.Gap as Gap
import Outputable (PprStyle)
import TcHsSyn (hsPatType)
import OccName (OccName)
import qualified Language.Haskell.Exts.Annotated as HE

----------------------------------------------------------------

instance HasType (LHsExpr Id) where
    getType tcm e = do
        hs_env <- G.getSession
        mbe <- liftIO $ Gap.deSugar tcm e hs_env
        return $ (G.getLoc e, ) <$> CoreUtils.exprType <$> mbe

instance HasType (LPat Id) where
    getType _ (G.L spn pat) = return $ Just (spn, hsPatType pat)

----------------------------------------------------------------

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
typeSigInRangeHE lineNo colNo (HE.TypeSig (HE.SrcSpanInfo s _)  _ _) =
  HE.srcSpanStart s <= (lineNo, colNo) && HE.srcSpanEnd s >= (lineNo, colNo)
typeSigInRangeHE _  _ _= False

pretty :: DynFlags -> PprStyle -> Type -> String
pretty dflag style = showOneLine dflag style . Gap.typeForUser

----------------------------------------------------------------

inModuleContext :: FilePath -> (DynFlags -> PprStyle -> Ghc a) -> Ghc a
inModuleContext file action =
    withDynFlags (setWarnTypedHoles . setDeferTypeErrors . setNoWaringFlags) $ do
    setTargetFiles [file]
    Gap.withContext $ do
        dflag <- G.getSessionDynFlags
        style <- getStyle
        action dflag style

----------------------------------------------------------------

showName :: DynFlags -> PprStyle -> G.Name -> String
showName dflag style name = showOneLine dflag style $ Gap.nameForUser name

showOccName :: DynFlags -> PprStyle -> OccName -> String
showOccName dflag style name = showOneLine dflag style $ Gap.occNameForUser name
