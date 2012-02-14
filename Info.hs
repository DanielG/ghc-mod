{-# LANGUAGE CPP, Rank2Types, TupleSections #-}

module Info (infoExpr, typeExpr) where

import Cabal
import Control.Applicative hiding (empty)
import Control.Exception
import Control.Monad
import CoreUtils
import Data.Generics as G
import Data.List
import Data.Maybe
import Data.Ord as O
import Desugar
import GHC
import HscTypes
import NameSet
import Outputable
import PprTyThing
import StringBuffer
import System.Time
import TcRnTypes
import Types

#if __GLASGOW_HASKELL__ >= 702
import CoreMonad
#endif

type Expression = String
type ModuleString = String

----------------------------------------------------------------

infoExpr :: Options -> ModuleString -> Expression -> FilePath -> IO String
infoExpr opt modstr expr file = (++ "\n") <$> info opt file modstr expr

info :: Options -> FilePath -> ModuleString -> FilePath -> IO String
info opt fileName modstr expr = inModuleContext opt fileName modstr exprToInfo
  where
    exprToInfo = infoThing expr

----------------------------------------------------------------

typeExpr :: Options -> ModuleString -> Int -> Int -> FilePath -> IO String
typeExpr opt modstr lineNo colNo file = (++ "\n") <$> Info.typeOf opt file modstr lineNo colNo

typeOf :: Options -> FilePath -> ModuleString -> Int -> Int -> IO String
typeOf opt fileName modstr lineNo colNo = inModuleContext opt fileName modstr exprToType
  where
    exprToType = do
      modSum <- getModSummary $ mkModuleName modstr
      p <- parseModule modSum
      tcm <- typecheckModule p
      es <- liftIO $ findExpr tcm lineNo colNo
      ts <- catMaybes <$> mapM (getType tcm) es
      let ts' = sortBy (\a b -> fst a `cmp` fst b) ts
      return $ tolisp $ map (\(loc, e) -> ("(" ++ l loc ++ " " ++ show (pretty e) ++ ")")) ts'

    l :: SrcSpan -> String
#if __GLASGOW_HASKELL__ >= 702
    l (RealSrcSpan spn)
#else
    l spn | isGoodSrcSpan spn
#endif
      = unwords . map show $
        [ srcSpanStartLine spn, srcSpanStartCol spn
        , srcSpanEndLine spn, srcSpanEndCol spn ]
    l _ = "0 0 0 0"
    
    cmp a b
      | a `isSubspanOf` b = O.LT
      | b `isSubspanOf` a = O.GT
      | otherwise = O.EQ
    
    tolisp ls = "(" ++ unwords ls ++ ")"

findExpr :: TypecheckedModule -> Int -> Int -> IO [LHsExpr Id]
findExpr tcm line col = do
  let src = tm_typechecked_source tcm
  ssrc <- everywhereM' sanitize src
  return $ listify f ssrc
  where
    -- It is for GHC's panic!
    sanitize :: Data a => a -> IO a
    sanitize x = do
      mret <- try (evaluate x)
      return $ case mret of
        Left (SomeException _) -> G.empty
        Right ret -> ret
    
    f :: LHsExpr Id -> Bool
    f (L spn _) = spn `spans'` (line, col)
    
    spans' spn loc
      = isGoodSrcSpan spn &&
        spn `spans` loc

-- | Monadic variation on everywhere'
everywhereM' :: Monad m => GenericM m -> GenericM m
everywhereM' f x = do
  x' <- f x
  gmapM (everywhereM' f) x'

getType :: GhcMonad m => TypecheckedModule -> LHsExpr Id -> m (Maybe (SrcSpan, Type))
getType tcm e = do
  hs_env <- getSession
  (_, mbe) <- liftIO $ deSugarExpr hs_env modu rn_env ty_env e
  return $ (getLoc e, ) <$> CoreUtils.exprType <$> mbe
  where
    modu = ms_mod $ pm_mod_summary $ tm_parsed_module tcm
    rn_env = tcg_rdr_env $ fst $ tm_internals_ tcm
    ty_env = tcg_type_env $ fst $ tm_internals_ tcm

pretty :: Type -> String
pretty = showSDocForUser neverQualify . pprTypeForUser False

----------------------------------------------------------------
-- from ghc/InteractiveUI.hs

infoThing :: String -> Ghc String
infoThing str = do
    names <- parseName str
    mb_stuffs <- mapM getInfo names
    let filtered = filterOutChildren (\(t,_f,_i) -> t) (catMaybes mb_stuffs)
    unqual <- getPrintUnqual
    return $ showSDocForUser unqual $ vcat (intersperse (text "") $ map (pprInfo False) filtered)

filterOutChildren :: (a -> TyThing) -> [a] -> [a]
filterOutChildren get_thing xs
  = [x | x <- xs, not (getName (get_thing x) `elemNameSet` implicits)]
  where
      implicits = mkNameSet [getName t | x <- xs, t <- implicitTyThings (get_thing x)]

pprInfo :: PrintExplicitForalls -> (TyThing, GHC.Fixity, [Instance]) -> SDoc
pprInfo pefas (thing, fixity, insts)
    = pprTyThingInContextLoc pefas thing
   $$ show_fixity fixity
   $$ vcat (map pprInstance insts)
  where
    show_fixity fix
        | fix == defaultFixity = Outputable.empty
        | otherwise            = ppr fix <+> ppr (getName thing)

----------------------------------------------------------------

inModuleContext :: Options -> FilePath -> ModuleString -> Ghc String -> IO String
inModuleContext opt fileName modstr action = withGHC valid
  where
    valid = do
        (file,_) <- initializeGHC opt fileName ["-w"] False
        setTargetFile file
        load LoadAllTargets
        mif setContextFromTarget action invalid
    invalid = do
        initializeGHC opt fileName ["-w"] False
        setTargetBuffer
        load LoadAllTargets
        mif setContextFromTarget action (return errorMessage)
    setTargetBuffer = do
        modgraph <- depanal [mkModuleName modstr] True
        let imports = concatMap (map (showSDoc . ppr . unLoc)) $
                      map ms_imps modgraph ++ map ms_srcimps modgraph
            moddef = "module " ++ sanitize modstr ++ " where"
            header = moddef : imports
#if __GLASGOW_HASKELL__ >= 702
            importsBuf = stringToStringBuffer . unlines $ header
#else
        importsBuf <- liftIO . stringToStringBuffer . unlines $ header
#endif
        clkTime <- liftIO getClockTime
        setTargets [Target (TargetModule $ mkModuleName modstr) True (Just (importsBuf, clkTime))]
    mif m t e = m >>= \ok -> if ok then t else e
    sanitize = fromMaybe "SomeModule" . listToMaybe . words
    errorMessage = "Couldn't determine type"

setContextFromTarget :: Ghc Bool
setContextFromTarget = do
    ms <- depanal [] False

#if __GLASGOW_HASKELL__ >= 704
    top <- map (IIModule . ms_mod) <$> filterM isTop ms
    setContext top
#else
    top <- map ms_mod <$> filterM isTop ms
    setContext top []
#endif
    return (not . null $ top)
  where
    isTop ms = lookupMod `gcatch` returnFalse
      where
        lookupMod = lookupModule (ms_mod_name ms) Nothing >> return True
        returnFalse = constE $ return False

----------------------------------------------------------------

constE :: a -> (SomeException -> a)
constE func = \_ -> func
