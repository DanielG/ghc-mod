module Language.Haskell.GhcMod.Browse (
    browse
  ) where

import Control.Applicative ((<$>))
import Control.Exception (SomeException(..))
import Data.Char (isAlpha)
import Data.List (sort)
import Data.Maybe (catMaybes)
import Exception (ghandle)
import FastString (mkFastString)
import GHC (GhcException(CmdLineError), ModuleInfo, Name, TyThing, DynFlags, Type, TyCon)
import qualified GHC as G
import Language.Haskell.GhcMod.Convert
import Language.Haskell.GhcMod.Doc (showPage, styleUnqualified)
import Language.Haskell.GhcMod.Gap
import Language.Haskell.GhcMod.Monad (GhcModT, options)
import Language.Haskell.GhcMod.Target (setTargetFiles)
import Language.Haskell.GhcMod.Types
import Name (getOccString)
import Outputable (ppr, Outputable)
import TyCon (isAlgTyCon)
import Type (dropForAlls, splitFunTy_maybe, mkFunTy, isPredTy)

----------------------------------------------------------------

-- | Getting functions, classes, etc from a module.
--   If 'detailed' is 'True', their types are also obtained.
--   If 'operators' is 'True', operators are also returned.
browse :: IOish m
       => ModuleString -- ^ A module name. (e.g. \"Data.List\")
       -> GhcModT m String
browse pkgmdl = convert' . sort =<< (listExports =<< getModule)
  where
    (mpkg,mdl) = splitPkgMdl pkgmdl
    mdlname = G.mkModuleName mdl
    mpkgid = mkFastString <$> mpkg
    listExports Nothing       = return []
    listExports (Just mdinfo) = processExports mdinfo
    -- findModule works only for package modules, moreover,
    -- you cannot load a package module. On the other hand,
    -- to browse a local module you need to load it first.
    -- If CmdLineError is signalled, we assume the user
    -- tried browsing a local module.
    getModule = browsePackageModule `G.gcatch` fallback `G.gcatch` handler
    browsePackageModule = G.findModule mdlname mpkgid >>= G.getModuleInfo
    browseLocalModule = ghandle handler $ do
      setTargetFiles [mdl]
      G.findModule mdlname Nothing >>= G.getModuleInfo
    fallback (CmdLineError _) = browseLocalModule
    fallback _                = return Nothing
    handler (SomeException _) = return Nothing
-- |
--
-- >>> splitPkgMdl "base:Prelude"
-- (Just "base","Prelude")
-- >>> splitPkgMdl "Prelude"
-- (Nothing,"Prelude")
splitPkgMdl :: String -> (Maybe String,String)
splitPkgMdl pkgmdl = case break (==':') pkgmdl of
    (mdl,"")    -> (Nothing,mdl)
    (pkg,_:mdl) -> (Just pkg,mdl)

processExports :: IOish m => ModuleInfo -> GhcModT m [String]
processExports minfo = do
  opt <- options
  let
    removeOps
      | operators opt = id
      | otherwise = filter (isAlpha . head . getOccString)
  mapM (showExport opt minfo) $ removeOps $ G.modInfoExports minfo

showExport :: IOish m => Options -> ModuleInfo -> Name -> GhcModT m String
showExport opt minfo e = do
  mtype' <- mtype
  return $ concat $ catMaybes [mqualified, Just $ formatOp $ getOccString e, mtype']
  where
    mqualified = (G.moduleNameString (G.moduleName $ G.nameModule e) ++ ".") `justIf` qualified opt
    mtype :: IOish m => GhcModT m (Maybe String)
    mtype
      | detailed opt = do
        tyInfo <- G.modInfoLookupName minfo e
        -- If nothing found, load dependent module and lookup global
        tyResult <- maybe (inOtherModule e) (return . Just) tyInfo
        dflag <- G.getSessionDynFlags
        return $ do
          typeName <- tyResult >>= showThing dflag
          (" :: " ++ typeName) `justIf` detailed opt
      | otherwise = return Nothing
    formatOp nm@(n:_)
      | isAlpha n = nm
      | otherwise = "(" ++ nm ++ ")"
    formatOp "" = error "formatOp"
    inOtherModule :: IOish m => Name -> GhcModT m (Maybe TyThing)
    inOtherModule nm = G.getModuleInfo (G.nameModule nm) >> G.lookupGlobalName nm
    justIf :: a -> Bool -> Maybe a
    justIf x True = Just x
    justIf _ False = Nothing

showThing :: DynFlags -> TyThing -> Maybe String
showThing dflag tything = showThing' dflag (fromTyThing tything)

showThing' :: DynFlags -> GapThing -> Maybe String
showThing' dflag (GtA a) = Just $ formatType dflag a
showThing' _     (GtT t) = unwords . toList <$> tyType t
  where
    toList t' = t' : getOccString t : map getOccString (G.tyConTyVars t)
showThing' _     _       = Nothing

formatType :: DynFlags -> Type -> String
formatType dflag a = showOutputable dflag (removeForAlls a)

tyType :: TyCon -> Maybe String
tyType typ
    | isAlgTyCon typ
      && not (G.isNewTyCon typ)
      && not (G.isClassTyCon typ) = Just "data"
    | G.isNewTyCon typ            = Just "newtype"
    | G.isClassTyCon typ          = Just "class"
    | G.isSynTyCon typ            = Just "type"
    | otherwise                   = Nothing

removeForAlls :: Type -> Type
removeForAlls ty = removeForAlls' ty' tty'
  where
    ty'  = dropForAlls ty
    tty' = splitFunTy_maybe ty'

removeForAlls' :: Type -> Maybe (Type, Type) -> Type
removeForAlls' ty Nothing = ty
removeForAlls' ty (Just (pre, ftype))
    | isPredTy pre        = mkFunTy pre (dropForAlls ftype)
    | otherwise           = ty

showOutputable :: Outputable a => DynFlags -> a -> String
showOutputable dflag = unwords . lines . showPage dflag styleUnqualified . ppr
