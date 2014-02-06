module Language.Haskell.GhcMod.Browse (browseModule, browse) where

import Control.Applicative
import Control.Monad (void)
import Data.Char
import Data.List
import Data.Maybe (catMaybes)
import DataCon (dataConRepType)
import FastString (mkFastString)
import GHC
import Panic (throwGhcException)
import Language.Haskell.GhcMod.Doc (showUnqualifiedPage)
import Language.Haskell.GhcMod.GHCApi
import Language.Haskell.GhcMod.Types
import Name
import Outputable
import TyCon
import Type
import Var

----------------------------------------------------------------

-- | Getting functions, classes, etc from a module.
--   If 'detailed' is 'True', their types are also obtained.
--   If 'operators' is 'True', operators are also returned.
browseModule :: Options
             -> Cradle
             -> ModuleString -- ^ A module name. (e.g. \"Data.List\")
             -> IO String
browseModule opt cradle mdlName = convert opt . sort <$> withGHCDummyFile (browse opt cradle mdlName)

-- | Getting functions, classes, etc from a module.
--   If 'detailed' is 'True', their types are also obtained.
--   If 'operators' is 'True', operators are also returned.
browse :: Options
       -> Cradle
       -> ModuleString -- ^ A module name. (e.g. \"Data.List\")
       -> Ghc [String]
browse opt cradle mdlName = do
    void $ initializeFlagsWithCradle opt cradle [] False
    getModule >>= getModuleInfo >>= listExports
  where
    getModule = findModule mdlname mpkgid `gcatch` fallback
    mdlname = mkModuleName mdlName
    mpkgid = mkFastString <$> packageId opt
    listExports Nothing       = return []
    listExports (Just mdinfo) = processExports opt mdinfo
    -- findModule works only for package modules, moreover,
    -- you cannot load a package module. On the other hand,
    -- to browse a local module you need to load it first.
    -- If CmdLineError is signalled, we assume the user
    -- tried browsing a local module.
    fallback (CmdLineError _) = loadAndFind
    fallback e                = throwGhcException e
    loadAndFind = do
      setTargetFiles [mdlName]
      checkSlowAndSet
      void $ load LoadAllTargets
      findModule mdlname Nothing

processExports :: Options -> ModuleInfo -> Ghc [String]
processExports opt minfo = mapM (showExport opt minfo) $ removeOps $ modInfoExports minfo
  where
    removeOps
      | operators opt = id
      | otherwise = filter (isAlpha . head . getOccString)

showExport :: Options -> ModuleInfo -> Name -> Ghc String
showExport opt minfo e = do
  mtype' <- mtype
  return $ concat $ catMaybes [mqualified, Just $ formatOp $ getOccString e, mtype']
  where
    mqualified = (moduleNameString (moduleName $ nameModule e) ++ ".") `justIf` qualified opt
    mtype
      | detailed opt = do
        tyInfo <- modInfoLookupName minfo e
        -- If nothing found, load dependent module and lookup global
        tyResult <- maybe (inOtherModule e) (return . Just) tyInfo
        dflag <- getSessionDynFlags
        return $ do
          typeName <- tyResult >>= showThing dflag
          (" :: " ++ typeName) `justIf` detailed opt
      | otherwise = return Nothing
    formatOp nm@(n:_)
      | isAlpha n = nm
      | otherwise = "(" ++ nm ++ ")"
    formatOp "" = error "formatOp"
    inOtherModule :: Name -> Ghc (Maybe TyThing)
    inOtherModule nm = getModuleInfo (nameModule nm) >> lookupGlobalName nm
    justIf :: a -> Bool -> Maybe a
    justIf x True = Just x
    justIf _ False = Nothing

showThing :: DynFlags -> TyThing -> Maybe String
showThing dflag (AnId i)     = Just $ formatType dflag varType i
showThing dflag (ADataCon d) = Just $ formatType dflag dataConRepType d
showThing _     (ATyCon t)   = unwords . toList <$> tyType t
  where
    toList t' = t' : getOccString t : map getOccString (tyConTyVars t)
showThing _     _            = Nothing

formatType :: NamedThing a => DynFlags -> (a -> Type) -> a -> String
formatType dflag f x = showOutputable dflag (removeForAlls $ f x)

tyType :: TyCon -> Maybe String
tyType typ
    | isAlgTyCon typ
      && not (isNewTyCon typ)
      && not (isClassTyCon typ) = Just "data"
    | isNewTyCon typ            = Just "newtype"
    | isClassTyCon typ          = Just "class"
    | isSynTyCon typ            = Just "type"
    | otherwise                 = Nothing

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
showOutputable dflag = unwords . lines . showUnqualifiedPage dflag . ppr
