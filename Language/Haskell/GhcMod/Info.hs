{-# LANGUAGE TupleSections, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE Rank2Types #-}

module Language.Haskell.GhcMod.Info (
    infoExpr
  , info
  , typeExpr
  , typeOf
  ) where

import Control.Applicative
import Control.Monad (void, when)
import CoreUtils
import Data.Function
import Data.Generics hiding (typeOf)
import Data.List
import Data.Maybe
import Data.Ord as O
import Data.Time.Clock
import Desugar
import GHC
import GHC.SYB.Utils
import HscTypes
import Language.Haskell.GhcMod.Doc
import Language.Haskell.GhcMod.GHCApi
import Language.Haskell.GhcMod.GHCChoice
import qualified Language.Haskell.GhcMod.Gap as Gap
import Language.Haskell.GhcMod.Types
import NameSet
import Outputable
import PprTyThing
import TcHsSyn (hsPatType)
import TcRnTypes

----------------------------------------------------------------

data Cmd = Info | Type deriving Eq

----------------------------------------------------------------

-- | Obtaining information of a target expression. (GHCi's info:)
infoExpr :: Options
         -> Cradle
         -> FilePath     -- ^ A target file
         -> ModuleString -- ^ A module name
         -> Expression   -- ^ A Haskell expression
         -> IO String
infoExpr opt cradle file modstr expr = (++ "\n") <$> withGHCDummyFile (info opt cradle file modstr expr)

-- | Obtaining information of a target expression. (GHCi's info:)
info :: Options
     -> Cradle
     -> FilePath     -- ^ A target file
     -> ModuleString -- ^ A module name
     -> Expression   -- ^ A Haskell expression
     -> Ghc String
info opt cradle file modstr expr =
    inModuleContext Info opt cradle file modstr exprToInfo "Cannot show info"
  where
    exprToInfo = infoThing expr

----------------------------------------------------------------

class HasType a where
    getType :: GhcMonad m => TypecheckedModule -> a -> m (Maybe (SrcSpan, Type))

instance HasType (LHsExpr Id) where
    getType tcm e = do
        hs_env <- getSession
        (_, mbe) <- Gap.liftIO $ deSugarExpr hs_env modu rn_env ty_env e
        return $ (getLoc e, ) <$> CoreUtils.exprType <$> mbe
      where
        modu = ms_mod $ pm_mod_summary $ tm_parsed_module tcm
        rn_env = tcg_rdr_env $ fst $ tm_internals_ tcm
        ty_env = tcg_type_env $ fst $ tm_internals_ tcm

instance HasType (LHsBind Id) where
    getType _ (L spn FunBind{fun_matches = MatchGroup _ typ}) = return $ Just (spn, typ)
    getType _ _ = return Nothing

instance HasType (LPat Id) where
    getType _ (L spn pat) = return $ Just (spn, hsPatType pat)

----------------------------------------------------------------

-- | Obtaining type of a target expression. (GHCi's type:)
typeExpr :: Options
         -> Cradle
         -> FilePath     -- ^ A target file
         -> ModuleString -- ^ A odule name
         -> Int          -- ^ Line number
         -> Int          -- ^ Column number
         -> IO String
typeExpr opt cradle file modstr lineNo colNo = withGHCDummyFile $ typeOf opt cradle file modstr lineNo colNo

-- | Obtaining type of a target expression. (GHCi's type:)
typeOf :: Options
       -> Cradle
       -> FilePath     -- ^ A target file
       -> ModuleString -- ^ A odule name
       -> Int          -- ^ Line number
       -> Int          -- ^ Column number
       -> Ghc String
typeOf opt cradle file modstr lineNo colNo =
    inModuleContext Type opt cradle file modstr exprToType errmsg
  where
    exprToType = do
      modSum <- getModSummary $ mkModuleName modstr
      p <- parseModule modSum
      tcm@TypecheckedModule{tm_typechecked_source = tcs} <- typecheckModule p
      let bs = listifySpans tcs (lineNo, colNo) :: [LHsBind Id]
          es = listifySpans tcs (lineNo, colNo) :: [LHsExpr Id]
          ps = listifySpans tcs (lineNo, colNo) :: [LPat Id]
      bts <- mapM (getType tcm) bs
      ets <- mapM (getType tcm) es
      pts <- mapM (getType tcm) ps
      dflag <- getSessionDynFlags
      let sss = map (toTup dflag) $ sortBy (cmp `on` fst) $ catMaybes $ concat [ets, bts, pts]
      return $ convert opt sss

    toTup :: DynFlags -> (SrcSpan, Type) -> ((Int,Int,Int,Int),String)
    toTup dflag (spn, typ) = (fourInts spn, pretty dflag typ)

    fourInts :: SrcSpan -> (Int,Int,Int,Int)
    fourInts = fromMaybe (0,0,0,0) . Gap.getSrcSpan

    cmp a b
      | a `isSubspanOf` b = O.LT
      | b `isSubspanOf` a = O.GT
      | otherwise = O.EQ

    errmsg = convert opt ([] :: [((Int,Int,Int,Int),String)])

listifySpans :: Typeable a => TypecheckedSource -> (Int, Int) -> [Located a]
listifySpans tcs lc = listifyStaged TypeChecker p tcs
  where
    p (L spn _) = isGoodSrcSpan spn && spn `spans` lc

listifyStaged :: Typeable r => Stage -> (r -> Bool) -> GenericQ [r]
listifyStaged s p = everythingStaged s (++) [] ([] `mkQ` (\x -> [x | p x]))

pretty :: DynFlags -> Type -> String
pretty dflag = showUnqualifiedOneLine dflag . pprTypeForUser False

----------------------------------------------------------------
-- from ghc/InteractiveUI.hs

infoThing :: String -> Ghc String
infoThing str = do
    names <- parseName str
    mb_stuffs <- mapM getInfo names
    let filtered = filterOutChildren (\(t,_f,_i) -> t) (catMaybes mb_stuffs)
    dflag <- getSessionDynFlags
    return $ showUnqualifiedPage dflag $ vcat (intersperse (text "") $ map (pprInfo False) filtered)

filterOutChildren :: (a -> TyThing) -> [a] -> [a]
filterOutChildren get_thing xs
    = [x | x <- xs, not (getName (get_thing x) `elemNameSet` implicits)]
  where
    implicits = mkNameSet [getName t | x <- xs, t <- implicitTyThings (get_thing x)]

pprInfo :: PrintExplicitForalls -> (TyThing, GHC.Fixity, [Gap.ClsInst]) -> SDoc
pprInfo pefas (thing, fixity, insts)
    = pprTyThingInContextLoc pefas thing
   $$ show_fixity fixity
   $$ vcat (map pprInstance insts)
  where
    show_fixity fx
      | fx == defaultFixity = Outputable.empty
      | otherwise           = ppr fx <+> ppr (getName thing)

----------------------------------------------------------------

inModuleContext :: Cmd -> Options -> Cradle -> FilePath -> ModuleString -> Ghc String -> String -> Ghc String
inModuleContext cmd opt cradle file modstr action errmsg =
    valid ||> invalid ||> return errmsg
  where
    valid = do
        void $ initializeFlagsWithCradle opt cradle ["-w:"] False
        when (cmd == Info) setSlowDynFlags
        setTargetFile file
        checkSlowAndSet
        void $ load LoadAllTargets
        doif setContextFromTarget action
    invalid = do
        void $ initializeFlagsWithCradle opt cradle ["-w:"] False
        setTargetBuffer
        checkSlowAndSet
        void $ load LoadAllTargets
        doif setContextFromTarget action
    setTargetBuffer = do
        modgraph <- depanal [mkModuleName modstr] True
        dflag <- getSessionDynFlags
        let imports = concatMap (map (showQualifiedPage dflag . ppr . unLoc)) $
                      map ms_imps modgraph ++ map ms_srcimps modgraph
            moddef = "module " ++ sanitize modstr ++ " where"
            header = moddef : imports
        importsBuf <- Gap.toStringBuffer header
        clkTime <- Gap.liftIO getCurrentTime
        setTargets [Gap.mkTarget (TargetModule $ mkModuleName modstr)
                                 True
                                 (Just (importsBuf, clkTime))]
    doif m t = m >>= \ok -> if ok then t else goNext
    sanitize = fromMaybe "SomeModule" . listToMaybe . words

setContextFromTarget :: Ghc Bool
setContextFromTarget = depanal [] False >>= Gap.setCtx
