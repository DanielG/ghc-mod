{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, CPP #-}

module Language.Haskell.GhcMod.Gap (
    Language.Haskell.GhcMod.Gap.ClsInst
  , mkTarget
  , withStyle
  , setLogAction
  , supportedExtensions
  , getSrcSpan
  , getSrcFile
  , setCtx
  , fOptions
  , toStringBuffer
  , liftIO
  , showSeverityCaption
  , setCabalPkg
  , addDevPkgs
  , filterOutChildren
  , infoThing
  , pprInfo
  , HasType(..)
#if __GLASGOW_HASKELL__ >= 702
#else
  , module Pretty
#endif
  ) where

import Control.Applicative hiding (empty)
import Control.Monad
import Data.List
import Data.Maybe
import Data.Time.Clock
import DynFlags
import ErrUtils
import FastString
import HscTypes
import Language.Haskell.GhcMod.GHCChoice
import Language.Haskell.GhcMod.Types hiding (convert)
import NameSet
import Outputable
import PprTyThing
import StringBuffer
import TcType

import qualified InstEnv
import qualified Pretty
import qualified StringBuffer as SB
#if __GLASGOW_HASKELL__ >= 707
import FamInstEnv
#endif

#if __GLASGOW_HASKELL__ >= 706
import GHC hiding (ClsInst)
#else
import GHC hiding (Instance)
#endif

#if __GLASGOW_HASKELL__ >= 702
import CoreMonad (liftIO)
#else
import HscTypes (liftIO)
import Pretty
#endif


#if __GLASGOW_HASKELL__ < 706
import Control.Arrow hiding ((<+>))
import Data.Convertible
#endif

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

----------------------------------------------------------------
----------------------------------------------------------------

supportedExtensions :: [String]
#if __GLASGOW_HASKELL__ >= 700
supportedExtensions = supportedLanguagesAndExtensions
#else
supportedExtensions = supportedLanguages
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

toStringBuffer :: [String] -> Ghc StringBuffer
#if __GLASGOW_HASKELL__ >= 702
toStringBuffer = return . stringToStringBuffer . unlines
#else
toStringBuffer = liftIO . stringToStringBuffer . unlines
#endif

----------------------------------------------------------------

fOptions :: [String]
#if __GLASGOW_HASKELL__ >= 704
fOptions = [option | (option,_,_) <- fFlags]
        ++ [option | (option,_,_) <- fWarningFlags]
        ++ [option | (option,_,_) <- fLangFlags]
#elif __GLASGOW_HASKELL__ == 702
fOptions = [option | (option,_,_,_) <- fFlags]
#else
fOptions = [option | (option,_,_) <- fFlags]
#endif

----------------------------------------------------------------
----------------------------------------------------------------

setCtx :: [ModSummary] -> Ghc Bool
#if __GLASGOW_HASKELL__ >= 704
setCtx ms = do
#if __GLASGOW_HASKELL__ >= 706
    let modName = IIModule . moduleName . ms_mod
#else
    let modName = IIModule . ms_mod
#endif
    top <- map modName <$> filterM isTop ms
    setContext top
    return (not . null $ top)
#else
setCtx ms = do
    top <- map ms_mod <$> filterM isTop ms
    setContext top []
    return (not . null $ top)
#endif
  where
    isTop mos = lookupMod ||> returnFalse
      where
        lookupMod = lookupModule (ms_mod_name mos) Nothing >> return True
        returnFalse = return False


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
#if __GLASGOW_HASKELL__ >= 707
setCabalPkg dflag = gopt_set dflag Opt_BuildingCabalPackage
#else
setCabalPkg dflag = dopt_set dflag Opt_BuildingCabalPackage
#endif

----------------------------------------------------------------

addDevPkgs :: DynFlags -> [Package] -> DynFlags
addDevPkgs df []   = df
addDevPkgs df pkgs = df''
  where
#if __GLASGOW_HASKELL__ >= 707
    df' = gopt_set df Opt_HideAllPackages
#else
    df' = dopt_set df Opt_HideAllPackages
#endif
    df'' = df' {
        packageFlags = map ExposePackage pkgs ++ packageFlags df
      }

----------------------------------------------------------------
----------------------------------------------------------------

class HasType a where
    getType :: GhcMonad m => TypecheckedModule -> a -> m (Maybe (SrcSpan, Type))


instance HasType (LHsBind Id) where
#if __GLASGOW_HASKELL__ >= 707
    getType _ (L spn FunBind{fun_matches = MG _ in_tys out_typ}) = return $ Just (spn, typ)
      where typ = mkFunTys in_tys out_typ
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

infoThing :: String -> Ghc SDoc
infoThing str = do
    names <- parseName str
#if __GLASGOW_HASKELL__ >= 707
    mb_stuffs <- mapM (getInfo False) names
    let filtered = filterOutChildren (\(t,_f,_i,_fam) -> t) (catMaybes mb_stuffs)
#else
    mb_stuffs <- mapM getInfo names
    let filtered = filterOutChildren (\(t,_f,_i) -> t) (catMaybes mb_stuffs)
#endif
    return $ vcat (intersperse (text "") $ map (pprInfo False) filtered)

#if __GLASGOW_HASKELL__ >= 707
pprInfo :: PrintExplicitForalls -> (TyThing, GHC.Fixity, [ClsInst], [FamInst]) -> SDoc
pprInfo pefas (thing, fixity, insts, famInsts)
    = pprTyThingInContextLoc pefas thing
   $$ show_fixity fixity
   $$ InstEnv.pprInstances insts
   $$ pprFamInsts famInsts
  where
    show_fixity fx
      | fx == defaultFixity = Outputable.empty
      | otherwise           = ppr fx <+> ppr (getName thing)
#else
pprInfo :: PrintExplicitForalls -> (TyThing, GHC.Fixity, [ClsInst]) -> SDoc
pprInfo pefas (thing, fixity, insts)
    = pprTyThingInContextLoc pefas thing
   $$ show_fixity fixity
   $$ vcat (map pprInstance insts)
  where
    show_fixity fx
      | fx == defaultFixity = Outputable.empty
      | otherwise           = ppr fx <+> ppr (getName thing)
#endif
