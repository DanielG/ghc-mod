{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, CPP, ScopedTypeVariables, RankNTypes #-}

module GhcMod.Gap (
    GhcMod.Gap.ClsInst
  , mkTarget
  , withStyle
  , GmLogAction
  , setLogAction
  , getSrcSpan
  , getSrcFile
  , withInteractiveContext
  , ghcCmdOptions
  , toStringBuffer
  , showSeverityCaption
  , setCabalPkg
  , setHideAllPackages
  , setDeferTypeErrors
  , setDeferTypedHoles
  , setWarnTypedHoles
  , setDumpSplices
  , setNoMaxRelevantBindings
  , isDumpSplices
  , filterOutChildren
  , infoThing
  , pprInfo
  , HasType(..)
  , errorMsgSpan
  , setErrorMsgSpan
  , typeForUser
  , nameForUser
  , occNameForUser
  , deSugar
  , showDocWith
  , renderGm
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
  , GhcMod.Gap.isSynTyCon
  , parseModuleHeader
  , mkErrStyle'
  , everythingStagedWithContext
  , withCleanupSession
  ) where

import Control.Applicative hiding (empty)
import Control.Monad (filterM)
import CoreSyn (CoreExpr)
import Data.List (intersperse)
import Data.Maybe (catMaybes)
import Data.Time.Clock (UTCTime)
import Data.Traversable hiding (mapM)
import DataCon (dataConUserType)
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
import SysTools
#if __GLASGOW_HASKELL__ >= 800
import GHCi (stopIServ)
#endif

import qualified Name
import qualified InstEnv
import qualified Pretty
import qualified StringBuffer as SB

#if __GLASGOW_HASKELL__ >= 710
import CoAxiom (coAxiomTyCon)
#endif

#if __GLASGOW_HASKELL__ >= 708
import FamInstEnv
import ConLike (ConLike(..))
import PatSyn
#else
import TcRnTypes
#endif

-- GHC 7.8 doesn't define this macro, nor does GHC 7.10.0
-- It IS defined from 7.10.1 and up though.
-- So we can only test for 7.10.1.0 and up with it.
#if __GLASGOW_HASKELL__ < 710
#ifndef MIN_VERSION_GLASGOW_HASKELL
#define MIN_VERSION_GLASGOW_HASKELL(a,b,c,d) FALSE
#endif
#endif

#if MIN_VERSION_GLASGOW_HASKELL(8,0,1,20161117)
import GHC hiding (ClsInst, withCleanupSession)
import qualified GHC (withCleanupSession)
#elif __GLASGOW_HASKELL__ >= 706
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

#if __GLASGOW_HASKELL__ < 706
import Control.DeepSeq (NFData(rnf))
import Data.ByteString.Lazy.Internal (ByteString(..))
#endif

import Bag
import Lexer as L
import Parser
import SrcLoc
import Packages
import Data.Generics (GenericQ, extQ, gmapQ)
import GHC.SYB.Utils (Stage(..))

import GhcMod.Types (Expression(..))
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

#if __GLASGOW_HASKELL__ >= 800
-- flip LogAction
type GmLogAction = WarnReason -> DynFlags -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()
#elif __GLASGOW_HASKELL__ >= 706
type GmLogAction = forall a. a -> LogAction
#else
type GmLogAction = forall a. a -> DynFlags -> LogAction
#endif

--  DynFlags -> WarnReason -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()

setLogAction :: DynFlags -> GmLogAction -> DynFlags
setLogAction df f =
#if __GLASGOW_HASKELL__ >= 800
    df { log_action = flip f }
#elif __GLASGOW_HASKELL__ >= 706
    df { log_action = f (error "setLogAction") }
#else
    df { log_action = f (error "setLogAction") df }
#endif

showDocWith :: DynFlags -> Pretty.Mode -> Pretty.Doc -> String
#if __GLASGOW_HASKELL__ >= 800
showDocWith dflags mode = Pretty.renderStyle mstyle where
    mstyle = Pretty.style { Pretty.mode = mode, Pretty.lineLength = pprCols dflags }
#elif __GLASGOW_HASKELL__ >= 708
-- Pretty.showDocWith disappeard.
-- https://github.com/ghc/ghc/commit/08a3536e4246e323fbcd8040e0b80001950fe9bc
showDocWith dflags mode = Pretty.showDoc mode (pprCols dflags)
#else
showDocWith _ = Pretty.showDocWith
#endif

renderGm :: Pretty.Doc -> String
#if __GLASGOW_HASKELL__ >= 800
renderGm = Pretty.fullRender Pretty.PageMode 80 1.2 string_txt ""
#else
renderGm = Pretty.fullRender Pretty.PageMode 80 1.2 string_txt ""
#endif
 where
   string_txt :: Pretty.TextDetails -> String -> String
   string_txt (Pretty.Chr c)   s  = c:s
   string_txt (Pretty.Str s1)  s2 = s1 ++ s2
   string_txt (Pretty.PStr s1) s2 = unpackFS s1 ++ s2
   string_txt (Pretty.LStr s1 _) s2 = unpackLitString s1 ++ s2
#if __GLASGOW_HASKELL__ >= 708
   string_txt (Pretty.ZStr s1) s2 = zString s1 ++ s2
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

ghcCmdOptions :: [String]
#if __GLASGOW_HASKELL__ >= 710
-- this also includes -X options and all sorts of other things so the
ghcCmdOptions = flagsForCompletion False
#else
ghcCmdOptions = [ "-f" ++ prefix ++ option
                | option <- opts
                , prefix <- ["","no-"]
                ]
#  if __GLASGOW_HASKELL__ >= 704
 where opts =
           [option | (option,_,_) <- fFlags]
        ++ [option | (option,_,_) <- fWarningFlags]
        ++ [option | (option,_,_) <- fLangFlags]
#  else
 where opts =
           [option | (option,_,_,_) <- fFlags]
        ++ [option | (option,_,_,_) <- fWarningFlags]
        ++ [option | (option,_,_,_) <- fLangFlags]
#  endif
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

withInteractiveContext :: GhcMonad m => m a -> m a
withInteractiveContext action = gbracket setup teardown body
  where
    setup = getContext
    teardown = setCtx
    body _ = do
        topImports >>= setCtx
        action
    topImports = do
        ms <- filterM moduleIsInterpreted =<< map ms_mod <$> getModuleGraph
        let iis = map (IIModule . modName) ms
#if __GLASGOW_HASKELL__ >= 704
        return iis
#else
        return (iis,[])
#endif
#if __GLASGOW_HASKELL__ >= 706
    modName = moduleName
    setCtx = setContext
#elif __GLASGOW_HASKELL__ >= 704
    modName = id
    setCtx = setContext
#else
    modName = ms_mod
    setCtx = uncurry setContext
#endif

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

setDeferTypedHoles :: DynFlags -> DynFlags
#if __GLASGOW_HASKELL__ >= 710
setDeferTypedHoles dflag = gopt_set dflag Opt_DeferTypedHoles
#else
setDeferTypedHoles = id
#endif

setWarnTypedHoles :: DynFlags -> DynFlags
#if __GLASGOW_HASKELL__ >= 708
setWarnTypedHoles dflag = wopt_set dflag Opt_WarnTypedHoles
#else
setWarnTypedHoles = id
#endif

----------------------------------------------------------------

-- | Set 'DynFlags' equivalent to "-fno-max-relevant-bindings".
setNoMaxRelevantBindings :: DynFlags -> DynFlags
#if __GLASGOW_HASKELL__ >= 708
setNoMaxRelevantBindings df = df { maxRelevantBinds = Nothing }
#else
setNoMaxRelevantBindings = id
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
   $$ vcat (map pprInstance' insts)
   $$ vcat (map pprFamInst' famInsts)
#else
pprInfo :: (FilePath -> FilePath) -> PrintExplicitForalls -> (TyThing, GHC.Fixity, [ClsInst]) -> SDoc
pprInfo m pefas (thing, fixity, insts)
    = pprTyThingInContextLoc' pefas thing
   $$ show_fixity fixity
   $$ vcat (map pprInstance' insts)
#endif
  where
    show_fixity fx
      | fx == defaultFixity = Outputable.empty
      | otherwise           = ppr fx <+> ppr (getName thing)
#if __GLASGOW_HASKELL__ >= 708
    pprTyThingInContextLoc' thing' = showWithLoc (pprDefinedAt' thing') (pprTyThingInContext thing')
#if __GLASGOW_HASKELL__ >= 710
    pprFamInst' (FamInst { fi_flavor = DataFamilyInst rep_tc })
      = pprTyThingInContextLoc (ATyCon rep_tc)

    pprFamInst' (FamInst { fi_flavor = SynFamilyInst, fi_axiom = axiom
                        , fi_tys = lhs_tys, fi_rhs = rhs })
      = showWithLoc (pprDefinedAt' (getName axiom)) $
        hang (ptext (sLit "type instance") <+> pprTypeApp (coAxiomTyCon axiom) lhs_tys)
           2 (equals <+> ppr rhs)
#else
    pprFamInst' ispec = showWithLoc (pprDefinedAt' (getName ispec)) (pprFamInstHdr ispec)
#endif
#else
    pprTyThingInContextLoc' pefas' thing' = showWithLoc (pprDefinedAt' thing') (pprTyThingInContext pefas' thing')
#endif
    showWithLoc loc doc
        = hang doc 2 (char '\t' <> comment <+> loc)
        -- The tab tries to make them line up a bit
      where
        comment = ptext (sLit "--")
    pprInstance' ispec = hang (pprInstanceHdr ispec)
        2 (ptext (sLit "--") <+> pprDefinedAt' (getName ispec))
    pprDefinedAt' thing' = ptext (sLit "Defined") <+> pprNameDefnLoc' (getName thing')
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

setErrorMsgSpan :: ErrMsg -> SrcSpan -> ErrMsg
#if __GLASGOW_HASKELL__ >= 708
setErrorMsgSpan err s = err { errMsgSpan = s }
#else
setErrorMsgSpan err s = err { errMsgSpans = [s] }
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

data GapThing = GtA Type
              | GtT TyCon
              | GtN
#if __GLASGOW_HASKELL__ >= 800
              | GtPatSyn PatSyn
#endif

fromTyThing :: TyThing -> GapThing
fromTyThing (AnId i)                   = GtA $ varType i
#if __GLASGOW_HASKELL__ >= 708
fromTyThing (AConLike (RealDataCon d)) = GtA $ dataConUserType d
#if __GLASGOW_HASKELL__ >= 800
fromTyThing (AConLike (PatSynCon p))   = GtPatSyn p
#else
fromTyThing (AConLike (PatSynCon p))   = GtA $ patSynType p
#endif
#else
fromTyThing (ADataCon d)               = GtA $ dataConUserType d
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
#if __GLASGOW_HASKELL__ >= 800
-- Instance declarations of sort 'instance F (G a)'
getClass [L loc (ClsInstD (ClsInstDecl {cid_poly_ty = HsIB _ (L _ (HsForAllTy _ (L _ (HsAppTy (L _ (HsTyVar (L _ className))) _))))}))] = Just (className, loc)
-- Instance declarations of sort 'instance F G' (no variables)
getClass [L loc (ClsInstD (ClsInstDecl {cid_poly_ty = HsIB _ (L _ (HsAppTy (L _ (HsTyVar (L _ className))) _))}))] = Just (className, loc)
#elif __GLASGOW_HASKELL__ >= 710
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

mkErrStyle' :: DynFlags -> PrintUnqualified -> PprStyle
#if __GLASGOW_HASKELL__ >= 706
mkErrStyle' = Outputable.mkErrStyle
#else
mkErrStyle' _ = Outputable.mkErrStyle
#endif

#if __GLASGOW_HASKELL__ < 706
instance NFData ByteString where
  rnf Empty       = ()
  rnf (Chunk _ b) = rnf b
#endif

-- | Like 'everything', but avoid known potholes, based on the 'Stage' that
--   generated the Ast.
everythingStagedWithContext :: Stage -> s -> (r -> r -> r) -> r -> GenericQ (s -> (r, s)) -> GenericQ r
everythingStagedWithContext stage s0 f z q x
  | (const False
#if __GLASGOW_HASKELL__ <= 708
      `extQ` postTcType
#endif
      `extQ` fixity `extQ` nameSet) x = z
  | otherwise = foldl f r (gmapQ (everythingStagedWithContext stage s' f z q) x)
  where nameSet    = const (stage `elem` [Parser,TypeChecker]) :: NameSet -> Bool
#if __GLASGOW_HASKELL__ <= 708
        postTcType = const (stage<TypeChecker)                 :: PostTcType -> Bool
#endif
        fixity     = const (stage<Renamer)                     :: GHC.Fixity -> Bool
        (r, s') = q x s0

withCleanupSession :: GhcMonad m => m a -> m a
#if __GLASGOW_HASKELL__ >= 800
#if MIN_VERSION_GLASGOW_HASKELL(8,0,1,20161117)
withCleanupSession = GHC.withCleanupSession
#else
withCleanupSession ghc = ghc `gfinally` cleanup
  where
   cleanup = do
      hsc_env <- getSession
      let dflags = hsc_dflags hsc_env
      liftIO $ do
          cleanTempFiles dflags
          cleanTempDirs dflags
          stopIServ hsc_env
#endif
#else
withCleanupSession action = do
  df <- getSessionDynFlags
  GHC.defaultCleanupHandler df action
#endif
