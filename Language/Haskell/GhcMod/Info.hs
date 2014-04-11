{-# LANGUAGE TupleSections, FlexibleInstances, Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Haskell.GhcMod.Info (
    infoExpr
  , info
  , typeExpr
  , typeOf
  ) where

import Control.Applicative ((<$>))
import Control.Monad (void)
import CoreMonad (liftIO)
import CoreUtils (exprType)
import Data.Function (on)
import Data.Generics hiding (typeOf)
import Data.List (sortBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord as O
import Exception (ghandle, SomeException(..))
import GHC (Ghc, LHsBind, LHsExpr, LPat, Id, TypecheckedModule(..), DynFlags, SrcSpan, Type, Located, TypecheckedSource, GenLocated(L), LoadHowMuch(..))
import qualified GHC as G
import GHC.SYB.Utils (Stage(TypeChecker), everythingStaged)
import Language.Haskell.GhcMod.Doc (showPage, showOneLine, getStyle)
import Language.Haskell.GhcMod.GHCApi
import Language.Haskell.GhcMod.Gap (HasType(..))
import qualified Language.Haskell.GhcMod.Gap as Gap
import Language.Haskell.GhcMod.Types
import Outputable (PprStyle)
import TcHsSyn (hsPatType)

----------------------------------------------------------------

-- | Obtaining information of a target expression. (GHCi's info:)
infoExpr :: Options
         -> Cradle
         -> FilePath     -- ^ A target file.
         -> Expression   -- ^ A Haskell expression.
         -> IO String
infoExpr opt cradle file expr = (++ "\n") <$> withGHCDummyFile (info opt cradle file expr)

-- | Obtaining information of a target expression. (GHCi's info:)
info :: Options
     -> Cradle
     -> FilePath     -- ^ A target file.
     -> Expression   -- ^ A Haskell expression.
     -> Ghc String
info opt cradle file expr =
    inModuleContext opt cradle file exprToInfo "Cannot show info"
  where
    exprToInfo = do
        dflag <- G.getSessionDynFlags
        sdoc <- Gap.infoThing expr
        style <- getStyle
        return $ showPage dflag style sdoc

----------------------------------------------------------------

instance HasType (LHsExpr Id) where
    getType tcm e = do
        hs_env <- G.getSession
        mbe <- liftIO $ Gap.deSugar tcm e hs_env
        return $ (G.getLoc e, ) <$> CoreUtils.exprType <$> mbe

instance HasType (LPat Id) where
    getType _ (G.L spn pat) = return $ Just (spn, hsPatType pat)

----------------------------------------------------------------

-- | Obtaining type of a target expression. (GHCi's type:)
typeExpr :: Options
         -> Cradle
         -> FilePath     -- ^ A target file.
         -> Int          -- ^ Line number.
         -> Int          -- ^ Column number.
         -> IO String
typeExpr opt cradle file lineNo colNo = withGHCDummyFile $ typeOf opt cradle file lineNo colNo

-- | Obtaining type of a target expression. (GHCi's type:)
typeOf :: Options
       -> Cradle
       -> FilePath     -- ^ A target file.
       -> Int          -- ^ Line number.
       -> Int          -- ^ Column number.
       -> Ghc String
typeOf opt cradle file lineNo colNo =
    inModuleContext opt cradle file exprToType errmsg
  where
    exprToType = do
      modGraph <- G.getModuleGraph
      let ms = filter (\m -> G.ml_hs_file (G.ms_location m) == Just file) modGraph
          modSum = head ms
      srcSpanTypes <- getSrcSpanType modSum lineNo colNo
      dflag <- G.getSessionDynFlags
      style <- getStyle
      let tups = map (toTup dflag style) $ sortBy (cmp `on` fst) srcSpanTypes
      return $ convert opt tups

    errmsg = convert opt ([] :: [((Int,Int,Int,Int),String)])

getSrcSpanType :: G.ModSummary -> Int -> Int -> Ghc [(SrcSpan, Type)]
getSrcSpanType modSum lineNo colNo = do
    p <- G.parseModule modSum
    tcm@TypecheckedModule{tm_typechecked_source = tcs} <- G.typecheckModule p
    let bs = listifySpans tcs (lineNo, colNo) :: [LHsBind Id]
        es = listifySpans tcs (lineNo, colNo) :: [LHsExpr Id]
        ps = listifySpans tcs (lineNo, colNo) :: [LPat Id]
    bts <- mapM (getType tcm) bs
    ets <- mapM (getType tcm) es
    pts <- mapM (getType tcm) ps
    return $ catMaybes $ concat [ets, bts, pts]

listifySpans :: Typeable a => TypecheckedSource -> (Int, Int) -> [Located a]
listifySpans tcs lc = listifyStaged TypeChecker p tcs
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

pretty :: DynFlags -> PprStyle -> Type -> String
pretty dflag style = showOneLine dflag style . Gap.typeForUser

----------------------------------------------------------------

noWaringOptions :: [String]
noWaringOptions = ["-w:"]

inModuleContext :: Options -> Cradle -> FilePath -> Ghc String -> String -> Ghc String
inModuleContext opt cradle file action errmsg = ghandle handler $ do
    void $ initializeFlagsWithCradle opt cradle noWaringOptions False
    setTargetFiles [file]
    void $ G.load LoadAllTargets
    void $ G.depanal [] False >>= Gap.setCtx
    action
 where
   handler (SomeException _) = return errmsg
