{-# LANGUAGE TupleSections, FlexibleInstances, TypeSynonymInstances, CPP #-}
{-# LANGUAGE Rank2Types #-}
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
--import Data.Generics (Typeable, GenericQ, mkQ)
import Data.Generics hiding (typeOf)
import Data.List (sortBy)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Ord as O
import Data.Time.Clock (getCurrentTime)
import GHC (Ghc, LHsBind, LHsExpr, LPat, Id, TypecheckedModule(..), DynFlags, SrcSpan, Type, Located, TypecheckedSource, GenLocated(L), LoadHowMuch(..), TargetId(..))
import qualified GHC as G
import GHC.SYB.Utils (Stage(TypeChecker), everythingStaged)
import HscTypes (ms_imps)
import Language.Haskell.GhcMod.Doc (showPage, showOneLine, getStyle)
import Language.Haskell.GhcMod.GHCApi
import Language.Haskell.GhcMod.GHCChoice ((||>), goNext)
import Language.Haskell.GhcMod.Gap (HasType(..))
import qualified Language.Haskell.GhcMod.Gap as Gap
import Language.Haskell.GhcMod.Types
import Outputable (PprStyle, ppr)
import TcHsSyn (hsPatType)

----------------------------------------------------------------

data Cmd = Info | Type deriving Eq

----------------------------------------------------------------

-- | Obtaining information of a target expression. (GHCi's info:)
infoExpr :: Options
         -> Cradle
         -> FilePath     -- ^ A target file.
         -> ModuleString -- ^ A module name.
         -> Expression   -- ^ A Haskell expression.
         -> IO String
infoExpr opt cradle file modstr expr = (++ "\n") <$> withGHCDummyFile (info opt cradle file modstr expr)

-- | Obtaining information of a target expression. (GHCi's info:)
info :: Options
     -> Cradle
     -> FilePath     -- ^ A target file.
     -> ModuleString -- ^ A module name.
     -> Expression   -- ^ A Haskell expression.
     -> Ghc String
info opt cradle file modstr expr =
    inModuleContext Info opt cradle file modstr exprToInfo "Cannot show info"
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
         -> ModuleString -- ^ A module name.
         -> Int          -- ^ Line number.
         -> Int          -- ^ Column number.
         -> IO String
typeExpr opt cradle file modstr lineNo colNo = withGHCDummyFile $ typeOf opt cradle file modstr lineNo colNo

-- | Obtaining type of a target expression. (GHCi's type:)
typeOf :: Options
       -> Cradle
       -> FilePath     -- ^ A target file.
       -> ModuleString -- ^ A module name.
       -> Int          -- ^ Line number.
       -> Int          -- ^ Column number.
       -> Ghc String
typeOf opt cradle file modstr lineNo colNo =
    inModuleContext Type opt cradle file modstr exprToType errmsg
  where
    exprToType = do
      modSum <- G.getModSummary $ G.mkModuleName modstr
      p <- G.parseModule modSum
      tcm@TypecheckedModule{tm_typechecked_source = tcs} <- G.typecheckModule p
      let bs = listifySpans tcs (lineNo, colNo) :: [LHsBind Id]
          es = listifySpans tcs (lineNo, colNo) :: [LHsExpr Id]
          ps = listifySpans tcs (lineNo, colNo) :: [LPat Id]
      bts <- mapM (getType tcm) bs
      ets <- mapM (getType tcm) es
      pts <- mapM (getType tcm) ps
      dflag <- G.getSessionDynFlags
      style <- getStyle
      let sss = map (toTup dflag style) $ sortBy (cmp `on` fst) $ catMaybes $ concat [ets, bts, pts]
      return $ convert opt sss

    toTup :: DynFlags -> PprStyle -> (SrcSpan, Type) -> ((Int,Int,Int,Int),String)
    toTup dflag style (spn, typ) = (fourInts spn, pretty dflag style typ)

    fourInts :: SrcSpan -> (Int,Int,Int,Int)
    fourInts = fromMaybe (0,0,0,0) . Gap.getSrcSpan

    cmp a b
      | a `G.isSubspanOf` b = O.LT
      | b `G.isSubspanOf` a = O.GT
      | otherwise = O.EQ

    errmsg = convert opt ([] :: [((Int,Int,Int,Int),String)])

listifySpans :: Typeable a => TypecheckedSource -> (Int, Int) -> [Located a]
listifySpans tcs lc = listifyStaged TypeChecker p tcs
  where
    p (L spn _) = G.isGoodSrcSpan spn && spn `G.spans` lc

listifyStaged :: Typeable r => Stage -> (r -> Bool) -> GenericQ [r]
listifyStaged s p = everythingStaged s (++) [] ([] `mkQ` (\x -> [x | p x]))

pretty :: DynFlags -> PprStyle -> Type -> String
pretty dflag style = showOneLine dflag style . Gap.typeForUser

----------------------------------------------------------------

inModuleContext :: Cmd -> Options -> Cradle -> FilePath -> ModuleString -> Ghc String -> String -> Ghc String
inModuleContext _ opt cradle file modstr action errmsg =
    valid ||> invalid ||> return errmsg
  where
    valid = do
        void $ initializeFlagsWithCradle opt cradle ["-w:"] False
        setTargetFiles [file]
        void $ G.load LoadAllTargets
        doif setContextFromTarget action
    invalid = do
        void $ initializeFlagsWithCradle opt cradle ["-w:"] False
        setTargetBuffer
        void $ G.load LoadAllTargets
        doif setContextFromTarget action
    setTargetBuffer = do
        modgraph <- G.depanal [G.mkModuleName modstr] True
        dflag <- G.getSessionDynFlags
        style <- getStyle
        let imports = concatMap (map (showPage dflag style . ppr . G.unLoc)) $
                      map ms_imps modgraph ++ map G.ms_srcimps modgraph
            moddef = "module " ++ sanitize modstr ++ " where"
            header = moddef : imports
        importsBuf <- Gap.toStringBuffer header
        clkTime <- liftIO getCurrentTime
        G.setTargets [Gap.mkTarget (TargetModule $ G.mkModuleName modstr)
                                   True
                                   (Just (importsBuf, clkTime))]
    doif m t = m >>= \ok -> if ok then t else goNext
    sanitize = fromMaybe "SomeModule" . listToMaybe . words

setContextFromTarget :: Ghc Bool
setContextFromTarget = G.depanal [] False >>= Gap.setCtx
