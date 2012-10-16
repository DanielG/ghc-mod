{-# LANGUAGE TupleSections, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE Rank2Types #-}

module Info (infoExpr, typeExpr) where

import Cabal
import Control.Applicative
import CoreUtils
import Data.Function
import Data.Generics
import Data.List
import Data.Maybe
import Data.Ord as O
import Data.Time.Clock
import Desugar
import GHC
import GHC.SYB.Utils
import GHCApi
import GHCChoice
import qualified Gap
import HscTypes
import NameSet
import Outputable
import PprTyThing
import Pretty (showDocWith, Mode(OneLineMode))
import TcRnTypes
import TcHsSyn (hsPatType)
import Types

----------------------------------------------------------------

type Expression = String
type ModuleString = String

----------------------------------------------------------------

infoExpr :: Options -> ModuleString -> Expression -> FilePath -> IO String
infoExpr opt modstr expr file = (++ "\n") <$> info opt file modstr expr

info :: Options -> FilePath -> ModuleString -> FilePath -> IO String
info opt fileName modstr expr =
    inModuleContext opt fileName modstr exprToInfo "Cannot show info"
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

typeExpr :: Options -> ModuleString -> Int -> Int -> FilePath -> IO String
typeExpr opt modstr lineNo colNo file = Info.typeOf opt file modstr lineNo colNo

typeOf :: Options -> FilePath -> ModuleString -> Int -> Int -> IO String
typeOf opt fileName modstr lineNo colNo =
    inModuleContext opt fileName modstr exprToType errmsg
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
      let sss = map toTup $ sortBy (cmp `on` fst) $ catMaybes $ concat [ets, bts, pts]
      return $ convert opt sss

    toTup :: (SrcSpan, Type) -> ((Int,Int,Int,Int),String)
    toTup (spn, typ) = (fourInts spn, pretty typ)

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

pretty :: Type -> String
pretty = showDocWith OneLineMode . Gap.styleDoc (mkUserStyle neverQualify AllTheWay) . pprTypeForUser False

----------------------------------------------------------------
-- from ghc/InteractiveUI.hs

infoThing :: String -> Ghc String
infoThing str = do
    names <- parseName str
    mb_stuffs <- mapM getInfo names
    let filtered = filterOutChildren (\(t,_f,_i) -> t) (catMaybes mb_stuffs)
    unqual <- getPrintUnqual
    return $ Gap.showDocForUser unqual $ vcat (intersperse (text "") $ map (pprInfo False) filtered)

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

inModuleContext :: Options -> FilePath -> ModuleString -> Ghc String -> String -> IO String
inModuleContext opt fileName modstr action errmsg =
    withGHC (valid ||> invalid ||> return errmsg)
  where
    valid = do
        (file,_) <- initializeGHC opt fileName ["-w"] False
        setTargetFile file
        _ <- load LoadAllTargets
        doif setContextFromTarget action
    invalid = do
        _ <- initializeGHC opt fileName ["-w"] False
        setTargetBuffer
        _ <- load LoadAllTargets
        doif setContextFromTarget action
    setTargetBuffer = do
        modgraph <- depanal [mkModuleName modstr] True
        let imports = concatMap (map (Gap.showDoc . ppr . unLoc)) $
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
