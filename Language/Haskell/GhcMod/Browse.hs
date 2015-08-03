module Language.Haskell.GhcMod.Browse (
    browse
  ) where

import Control.Applicative
import Control.Exception (SomeException(..))
import Data.Char
import Data.List
import Data.Maybe
import FastString
import GHC
import qualified GHC as G
import Language.Haskell.GhcMod.Convert
import Language.Haskell.GhcMod.Doc (showPage, styleUnqualified)
import Language.Haskell.GhcMod.Gap as Gap
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Types
import Name (getOccString)
import Outputable
import TyCon (isAlgTyCon)
import Type (dropForAlls, splitFunTy_maybe, mkFunTy, isPredTy)
import Exception (ExceptionMonad, ghandle)
import Prelude

----------------------------------------------------------------

-- | Getting functions, classes, etc from a module.
--   If 'detailed' is 'True', their types are also obtained.
--   If 'operators' is 'True', operators are also returned.
browse :: forall m. IOish m
       => String -- ^ A module name. (e.g. \"Data.List\", "base:Prelude")
       -> GhcModT m String
browse pkgmdl = do
    convert' . sort =<< go
  where
    -- TODO: Add API to Gm.Target to check if module is home module without
    -- bringing up a GHC session as well then this can be made a lot cleaner
    go = ghandle (\(SomeException _) -> return []) $ do
      goPkgModule `G.gcatch` (\(SomeException _) -> goHomeModule)

    goPkgModule = do
      opt <- options
      runGmPkgGhc $
        processExports opt =<< tryModuleInfo =<< G.findModule mdlname mpkgid

    goHomeModule = runGmlT [Right mdlname] $ do
      opt <- options
      processExports opt =<< tryModuleInfo =<< G.findModule mdlname Nothing

    tryModuleInfo m = fromJust <$> G.getModuleInfo m

    (mpkg, mdl) = splitPkgMdl pkgmdl
    mdlname = G.mkModuleName mdl
    mpkgid = mkFastString <$> mpkg

-- |
--
-- >>> splitPkgMdl "base:Prelude"
-- (Just "base","Prelude")
-- >>> splitPkgMdl "Prelude"
-- (Nothing,"Prelude")
splitPkgMdl :: String -> (Maybe String,String)
splitPkgMdl pkgmdl =
  case break (==':') pkgmdl of
    (mdl, "")    -> (Nothing, mdl)
    (pkg, _:mdl) -> (Just pkg, mdl)

-- Haskell 2010:
-- small -> ascSmall | uniSmall | _
-- ascSmall -> a | b | ... | z
-- uniSmall -> any Unicode lowercase letter
-- varid -> (small {small | large | digit | ' })

isNotOp :: String -> Bool
isNotOp (h:_) = isAlpha h || (h == '_')
isNotOp _ = error "isNotOp"

processExports :: (G.GhcMonad m, MonadIO m, ExceptionMonad m)
               => Options -> ModuleInfo -> m [String]
processExports opt minfo = do
  let
    removeOps
      | operators opt = id
      | otherwise = filter (isNotOp . getOccString)
  mapM (showExport opt minfo) $ removeOps $ G.modInfoExports minfo

showExport :: forall m. (G.GhcMonad m, MonadIO m, ExceptionMonad m)
           => Options -> ModuleInfo -> Name -> m String
showExport opt minfo e = do
  mtype' <- mtype
  return $ concat $ catMaybes [mqualified, Just $ formatOp $ getOccString e, mtype']
  where
    mqualified = (G.moduleNameString (G.moduleName $ G.nameModule e) ++ ".") `justIf` qualified opt
    mtype :: m (Maybe String)
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
    formatOp nm
      | null nm    = error "formatOp"
      | isNotOp nm = nm
      | otherwise  = "(" ++ nm ++ ")"
    inOtherModule :: Name -> m (Maybe TyThing)
    inOtherModule nm = do
      G.getModuleInfo (G.nameModule nm) >> G.lookupGlobalName nm
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
    | Gap.isSynTyCon typ          = Just "type"
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
