{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, CPP, ScopedTypeVariables, RankNTypes #-}

module Language.Haskell.GhcMod.Gap (
    Language.Haskell.GhcMod.Gap.ClsInst
  , mkTarget
  , withStyle
  , setLogAction
  , getSrcSpan
  , getSrcFile
  , withContext
  , fOptions
  , toStringBuffer
  , showSeverityCaption
  , setCabalPkg
  , setHideAllPackages
  , setDeferTypeErrors
  , setWarnTypedHoles
  , setDumpSplices
  , isDumpSplices
  , filterOutChildren
  , infoThing
  , pprInfo
  , HasType(..)
  , errorMsgSpan
  , typeForUser
  , nameForUser
  , occNameForUser
  , deSugar
  , showDocWith
  , GapThing(..)
  , fromTyThing
  , fileModSummary
  , WarnFlags
  , emptyWarnFlags
  , GLMatch
  , GLMatchI
  , getClass
  , occName
  , listVisibleModuleNames
  , listVisibleModules
  , lookupModulePackageInAllPackages
  , Language.Haskell.GhcMod.Gap.isSynTyCon
  , parseModuleHeader
  ) where

import Control.Applicative hiding (empty)
import Control.Monad (filterM)
import CoreSyn (CoreExpr)
import Data.List (intersperse)
import Data.Maybe (catMaybes)
import Data.Time.Clock (UTCTime)
import Data.Traversable hiding (mapM)
import DataCon (dataConRepType)
import Desugar (deSugarExpr)
import DynFlags
import ErrUtils
import Exception
import FastString
import GhcMonad
import HscTypes
import NameSet
import OccName
import Outputable
import PprTyThing
import StringBuffer
import TcType
import Var (varType)
import System.Directory

import qualified Name
import qualified InstEnv
import qualified Pretty
import qualified StringBuffer as SB

#if __GLASGOW_HASKELL__ >= 708
import FamInstEnv
import ConLike (ConLike(..))
import PatSyn (patSynType)
#else
import TcRnTypes
#endif

#if __GLASGOW_HASKELL__ >= 706
import GHC hiding (ClsInst)
#else
import GHC hiding (Instance)
import Control.Arrow hiding ((<+>))
import Data.Convertible
import RdrName (rdrNameOcc)
#endif

#if __GLASGOW_HASKELL__ < 710
import UniqFM (eltsUFM)
import Module
#endif

#if __GLASGOW_HASKELL__ >= 704
import qualified Data.IntSet as I (IntSet, empty)
#endif

import Bag
import Lexer as L
import Parser
import SrcLoc
import Packages

import Language.Haskell.GhcMod.Types (Expression(..))
import Prelude

----------------------------------------------------------------
----------------------------------------------------------------
--
#if __GLASGOW_HASKELL__ >= 706
type ClsInst = InstEnv.ClsInst
#else
type ClsInst = InstEnv.Instance
#endif

mkTarget :: TargetId -> Bool -> Maybe (SB.StringBuffer, UTCTime) -> Target
#if __GLASGOW_HASKELL__ >= 706
mkTarget = Target
#else
mkTarget tid allowObjCode = Target tid allowObjCode . (fmap . second) convert
#endif

----------------------------------------------------------------
----------------------------------------------------------------

withStyle :: DynFlags -> PprStyle -> SDoc -> Pretty.Doc
#if __GLASGOW_HASKELL__ >= 706
withStyle = withPprStyleDoc
#else
withStyle _ = withPprStyleDoc
#endif

setLogAction :: DynFlags
             -> (DynFlags -> Severity -> SrcSpan -> PprStyle -> SDoc -> IO ())
             -> DynFlags
setLogAction df f =
#if __GLASGOW_HASKELL__ >= 706
    df { log_action = f }
#else
    df { log_action = f df }
#endif

showDocWith :: DynFlags -> Pretty.Mode -> Pretty.Doc -> String
#if __GLASGOW_HASKELL__ >= 708
-- Pretty.showDocWith disappeard.
-- https://github.com/ghc/ghc/commit/08a3536e4246e323fbcd8040e0b80001950fe9bc
showDocWith dflags mode = Pretty.showDoc mode (pprCols dflags)
#else
showDocWith _ = Pretty.showDocWith
#endif

----------------------------------------------------------------
----------------------------------------------------------------

getSrcSpan :: SrcSpan -> Maybe (Int,Int,Int,Int)
#if __GLASGOW_HASKELL__ >= 702
getSrcSpan (RealSrcSpan spn)
#else
getSrcSpan spn | isGoodSrcSpan spn
#endif
             = Just (srcSpanStartLine spn
                   , srcSpanStartCol spn
                   , srcSpanEndLine spn
                   , srcSpanEndCol spn)
getSrcSpan _ = Nothing

getSrcFile :: SrcSpan -> Maybe String
#if __GLASGOW_HASKELL__ >= 702
getSrcFile (RealSrcSpan spn)       = Just . unpackFS . srcSpanFile $ spn
#else
getSrcFile spn | isGoodSrcSpan spn = Just . unpackFS . srcSpanFile $ spn
#endif
getSrcFile _ = Nothing

----------------------------------------------------------------

toStringBuffer :: GhcMonad m => [String] -> m StringBuffer
#if __GLASGOW_HASKELL__ >= 702
toStringBuffer = return . stringToStringBuffer . unlines
#else
toStringBuffer = liftIO . stringToStringBuffer . unlines
#endif

----------------------------------------------------------------

fOptions :: [String]
#if __GLASGOW_HASKELL__ >= 710
fOptions = [option | (FlagSpec option _ _ _) <- fFlags]
        ++ [option | (FlagSpec option _ _ _) <- fWarningFlags]
        ++ [option | (FlagSpec option _ _ _) <- fLangFlags]
#elif __GLASGOW_HASKELL__ >= 704
fOptions = [option | (option,_,_) <- fFlags]
        ++ [option | (option,_,_) <- fWarningFlags]
        ++ [option | (option,_,_) <- fLangFlags]
#else
fOptions = [option | (option,_,_,_) <- fFlags]
        ++ [option | (option,_,_,_) <- fWarningFlags]
        ++ [option | (option,_,_,_) <- fLangFlags]
#endif

----------------------------------------------------------------
----------------------------------------------------------------

fileModSummary :: GhcMonad m => FilePath -> m ModSummary
fileModSummary file' = do
    mss <- getModuleGraph
    file <- liftIO $ canonicalizePath file'
    [ms] <- liftIO $ flip filterM mss $ \m ->
        (Just file==) <$> canonicalizePath `traverse` ml_hs_file (ms_location m)
    return ms

withContext :: GhcMonad m => m a -> m a
withContext action = gbracket setup teardown body
  where
    setup = getContext
    teardown = setCtx
    body _ = do
        topImports >>= setCtx
        action
    topImports = do
        mss <- getModuleGraph
        mns <- map modName <$> filterM isTop mss
        let ii = map IIModule mns
#if __GLASGOW_HASKELL__ >= 704
        return ii
#else
        return (ii,[])
#endif
    isTop mos = lookupMod mos ||> returnFalse
    lookupMod mos = lookupModule (ms_mod_name mos) Nothing >> return True
    returnFalse = return False
#if __GLASGOW_HASKELL__ >= 706
    modName = moduleName . ms_mod
    setCtx = setContext
#elif __GLASGOW_HASKELL__ >= 704
    modName = ms_mod
    setCtx = setContext
#else
    modName = ms_mod
    setCtx = uncurry setContext
#endif

-- | Try the left action, if an IOException occurs try the right action.
(||>) :: ExceptionMonad m => m a -> m a -> m a
x ||> y = x `gcatch` (\(_ :: IOException) -> y)

showSeverityCaption :: Severity -> String
#if __GLASGOW_HASKELL__ >= 706
showSeverityCaption SevWarning = "Warning: "
showSeverityCaption _          = ""
#else
showSeverityCaption = const ""
#endif

----------------------------------------------------------------
----------------------------------------------------------------

setCabalPkg :: DynFlags -> DynFlags
#if __GLASGOW_HASKELL__ >= 708
setCabalPkg dflag = gopt_set dflag Opt_BuildingCabalPackage
#else
setCabalPkg dflag = dopt_set dflag Opt_BuildingCabalPackage
#endif

----------------------------------------------------------------

setHideAllPackages :: DynFlags -> DynFlags
#if __GLASGOW_HASKELL__ >= 708
setHideAllPackages df = gopt_set df Opt_HideAllPackages
#else
setHideAllPackages df = dopt_set df Opt_HideAllPackages
#endif

----------------------------------------------------------------

setDumpSplices :: DynFlags -> DynFlags
setDumpSplices dflag = dopt_set dflag Opt_D_dump_splices

isDumpSplices :: DynFlags -> Bool
isDumpSplices dflag = dopt Opt_D_dump_splices dflag

----------------------------------------------------------------


setDeferTypeErrors :: DynFlags -> DynFlags
#if __GLASGOW_HASKELL__ >= 708
setDeferTypeErrors dflag = gopt_set dflag Opt_DeferTypeErrors
#elif __GLASGOW_HASKELL__ >= 706
setDeferTypeErrors dflag = dopt_set dflag Opt_DeferTypeErrors
#else
setDeferTypeErrors = id
#endif

setWarnTypedHoles :: DynFlags -> DynFlags
#if __GLASGOW_HASKELL__ >= 708
setWarnTypedHoles dflag = wopt_set dflag Opt_WarnTypedHoles
#else
setWarnTypedHoles = id
#endif

----------------------------------------------------------------
----------------------------------------------------------------

class HasType a where
    getType :: GhcMonad m => TypecheckedModule -> a -> m (Maybe (SrcSpan, Type))


instance HasType (LHsBind Id) where
#if __GLASGOW_HASKELL__ >= 708
    getType _ (L spn FunBind{fun_matches = m}) = return $ Just (spn, typ)
      where in_tys = mg_arg_tys m
            out_typ = mg_res_ty m
            typ = mkFunTys in_tys out_typ
#else
    getType _ (L spn FunBind{fun_matches = MatchGroup _ typ}) = return $ Just (spn, typ)
#endif
    getType _ _ = return Nothing

----------------------------------------------------------------
----------------------------------------------------------------
-- from ghc/InteractiveUI.hs

filterOutChildren :: (a -> TyThing) -> [a] -> [a]
filterOutChildren get_thing xs
    = [x | x <- xs, not (getName (get_thing x) `elemNameSet` implicits)]
  where
    implicits = mkNameSet [getName t | x <- xs, t <- implicitTyThings (get_thing x)]

infoThing :: GhcMonad m => (FilePath -> FilePath) -> Expression -> m SDoc
infoThing m (Expression str) = do
    names <- parseName str
#if __GLASGOW_HASKELL__ >= 708
    mb_stuffs <- mapM (getInfo False) names
    let filtered = filterOutChildren (\(t,_f,_i,_fam) -> t) (catMaybes mb_stuffs)
#else
    mb_stuffs <- mapM getInfo names
    let filtered = filterOutChildren (\(t,_f,_i) -> t) (catMaybes mb_stuffs)
#endif
    return $ vcat (intersperse (text "") $ map (pprInfo m False) filtered)

#if __GLASGOW_HASKELL__ >= 708
pprInfo :: (FilePath -> FilePath) -> Bool -> (TyThing, GHC.Fixity, [ClsInst], [FamInst]) -> SDoc
pprInfo m _ (thing, fixity, insts, famInsts)
    = pprTyThingInContextLoc' thing
   $$ show_fixity fixity
   $$ InstEnv.pprInstances insts
   $$ pprFamInsts famInsts
#else
pprInfo :: (FilePath -> FilePath) -> PrintExplicitForalls -> (TyThing, GHC.Fixity, [ClsInst]) -> SDoc
pprInfo m pefas (thing, fixity, insts)
    = pprTyThingInContextLoc' pefas thing
   $$ show_fixity fixity
   $$ vcat (map pprInstance insts)
#endif
  where
    show_fixity fx
      | fx == defaultFixity = Outputable.empty
      | otherwise           = ppr fx <+> ppr (getName thing)
#if __GLASGOW_HASKELL__ >= 708
    pprTyThingInContextLoc' thing' = hang (pprTyThingInContext thing') 2
                                        (char '\t' <> ptext (sLit "--") <+> loc)
                         where loc = ptext (sLit "Defined") <+> pprNameDefnLoc' (getName thing')
#else
    pprTyThingInContextLoc' pefas thing' = hang (pprTyThingInContext pefas thing') 2
                                    (char '\t' <> ptext (sLit "--") <+> loc)
                               where loc = ptext (sLit "Defined") <+> pprNameDefnLoc' (getName thing')
#endif
    pprNameDefnLoc' name
      = case Name.nameSrcLoc name of
           RealSrcLoc s -> ptext (sLit "at") <+> ppr (subst s)
           UnhelpfulLoc s
             | Name.isInternalName name || Name.isSystemName name
             -> ptext (sLit "at") <+> ftext s
             | otherwise
             -> ptext (sLit "in") <+> quotes (ppr (nameModule name))
      where subst s = mkRealSrcLoc (realFP s) (srcLocLine s) (srcLocCol s)
            realFP = mkFastString . m . unpackFS . srcLocFile

----------------------------------------------------------------
----------------------------------------------------------------

errorMsgSpan :: ErrMsg -> SrcSpan
#if __GLASGOW_HASKELL__ >= 708
errorMsgSpan = errMsgSpan
#else
errorMsgSpan = head . errMsgSpans
#endif

typeForUser :: Type -> SDoc
#if __GLASGOW_HASKELL__ >= 708
typeForUser = pprTypeForUser
#else
typeForUser = pprTypeForUser False
#endif

nameForUser :: Name -> SDoc
nameForUser = pprOccName . getOccName

occNameForUser :: OccName -> SDoc
occNameForUser = pprOccName

deSugar :: TypecheckedModule -> LHsExpr Id -> HscEnv
         -> IO (Maybe CoreExpr)
#if __GLASGOW_HASKELL__ >= 708
deSugar _   e hs_env = snd <$> deSugarExpr hs_env e
#else
deSugar tcm e hs_env = snd <$> deSugarExpr hs_env modu rn_env ty_env e
  where
    modu = ms_mod $ pm_mod_summary $ tm_parsed_module tcm
    tcgEnv = fst $ tm_internals_ tcm
    rn_env = tcg_rdr_env tcgEnv
    ty_env = tcg_type_env tcgEnv
#endif

----------------------------------------------------------------
----------------------------------------------------------------

data GapThing = GtA Type | GtT TyCon | GtN

fromTyThing :: TyThing -> GapThing
fromTyThing (AnId i)                   = GtA $ varType i
#if __GLASGOW_HASKELL__ >= 708
fromTyThing (AConLike (RealDataCon d)) = GtA $ dataConRepType d
fromTyThing (AConLike (PatSynCon p))   = GtA $ patSynType p
#else
fromTyThing (ADataCon d)               = GtA $ dataConRepType d
#endif
fromTyThing (ATyCon t)                 = GtT t
fromTyThing _                          = GtN

----------------------------------------------------------------
----------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 704
type WarnFlags = I.IntSet
emptyWarnFlags :: WarnFlags
emptyWarnFlags = I.empty
#else
type WarnFlags = [WarningFlag]
emptyWarnFlags :: WarnFlags
emptyWarnFlags = []
#endif

----------------------------------------------------------------
----------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 708
type GLMatch = LMatch RdrName (LHsExpr RdrName)
type GLMatchI = LMatch Id (LHsExpr Id)
#else
type GLMatch = LMatch RdrName
type GLMatchI = LMatch Id
#endif

getClass :: [LInstDecl Name] -> Maybe (Name, SrcSpan)
#if __GLASGOW_HASKELL__ >= 710
-- Instance declarations of sort 'instance F (G a)'
getClass [L loc (ClsInstD (ClsInstDecl {cid_poly_ty = (L _ (HsForAllTy _ _ _ _ (L _ (HsAppTy (L _ (HsTyVar className)) _))))}))] = Just (className, loc)
-- Instance declarations of sort 'instance F G' (no variables)
getClass [L loc (ClsInstD (ClsInstDecl {cid_poly_ty = (L _ (HsAppTy (L _ (HsTyVar className)) _))}))] = Just (className, loc)
#elif __GLASGOW_HASKELL__ >= 708
-- Instance declarations of sort 'instance F (G a)'
getClass [L loc (ClsInstD (ClsInstDecl {cid_poly_ty = (L _ (HsForAllTy _ _ _ (L _ (HsAppTy (L _ (HsTyVar className)) _))))}))] = Just (className, loc)
-- Instance declarations of sort 'instance F G' (no variables)
getClass [L loc (ClsInstD (ClsInstDecl {cid_poly_ty = (L _ (HsAppTy (L _ (HsTyVar className)) _))}))] = Just (className, loc)
#elif __GLASGOW_HASKELL__ >= 706
getClass [L loc (ClsInstD (L _ (HsForAllTy _ _ _ (L _ (HsAppTy (L _ (HsTyVar className)) _)))) _ _ _)] = Just (className, loc)
getClass[L loc (ClsInstD (L _ (HsAppTy (L _ (HsTyVar className)) _)) _ _ _)] = Just (className, loc)
#else
getClass [L loc (InstDecl (L _ (HsForAllTy _ _ _ (L _ (HsAppTy (L _ (HsTyVar className)) _)))) _ _ _)] = Just (className, loc)
getClass [L loc (InstDecl (L _ (HsAppTy (L _ (HsTyVar className)) _)) _ _ _)] = Just (className, loc)
#endif
getClass _ = Nothing

#if __GLASGOW_HASKELL__ < 706
occName :: RdrName -> OccName
occName = rdrNameOcc
#endif

----------------------------------------------------------------

#if __GLASGOW_HASKELL__ < 710
-- Copied from ghc/InteractiveUI.hs
allExposedPackageConfigs :: DynFlags -> [PackageConfig]
allExposedPackageConfigs df = filter exposed $ eltsUFM $ pkgIdMap $ pkgState df

allExposedModules :: DynFlags -> [ModuleName]
allExposedModules df = concat $ map exposedModules $ allExposedPackageConfigs df

listVisibleModuleNames :: DynFlags -> [ModuleName]
listVisibleModuleNames = allExposedModules
#endif

lookupModulePackageInAllPackages ::
    DynFlags -> ModuleName -> [String]
lookupModulePackageInAllPackages df mn =
#if __GLASGOW_HASKELL__ >= 710
    unpackSPId . sourcePackageId . snd <$> lookupModuleInAllPackages df mn
 where
   unpackSPId (SourcePackageId fs) = unpackFS fs
#else
    unpackPId . sourcePackageId . fst <$> lookupModuleInAllPackages df mn
 where
   unpackPId pid = packageIdString $ mkPackageId pid
--       n ++ "-" ++ showVersion v
#endif

listVisibleModules :: DynFlags -> [GHC.Module]
listVisibleModules df = let
#if __GLASGOW_HASKELL__ >= 710
    modNames = listVisibleModuleNames df
    mods = [ m | mn <- modNames, (m, _) <- lookupModuleInAllPackages df mn ]
#else
    pkgCfgs = allExposedPackageConfigs df
    mods = [ mkModule pid modname | p <- pkgCfgs
           , let pid = packageConfigId p
           , modname <- exposedModules p ]
#endif
    in mods

isSynTyCon :: TyCon -> Bool
#if __GLASGOW_HASKELL__ >= 710
isSynTyCon = GHC.isTypeSynonymTyCon
#else
isSynTyCon = GHC.isSynTyCon
#endif


parseModuleHeader
    :: String         -- ^ Haskell module source text (full Unicode is supported)
    -> DynFlags
    -> FilePath       -- ^ the filename (for source locations)
    -> Either ErrorMessages (WarningMessages, Located (HsModule RdrName))
parseModuleHeader str dflags filename =
   let
       loc  = mkRealSrcLoc (mkFastString filename) 1 1
       buf  = stringToStringBuffer str
   in
   case L.unP Parser.parseHeader (mkPState dflags buf loc) of

     PFailed sp err   ->
#if __GLASGOW_HASKELL__ >= 706
         Left (unitBag (mkPlainErrMsg dflags sp err))
#else
         Left (unitBag (mkPlainErrMsg sp err))
#endif

     POk pst rdr_module ->
         let (warns,_) = getMessages pst in
         Right (warns, rdr_module)
