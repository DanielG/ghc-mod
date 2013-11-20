module Language.Haskell.GhcMod.Browse (browseModule, browse) where

import Control.Applicative
import Control.Monad (void)
import Data.Char
import Data.List
import Data.Maybe (fromMaybe)
import DataCon (dataConRepType)
import GHC
import Panic(throwGhcException)
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
browseModule opt cradle mdlName = convert opt . format <$> withGHCDummyFile (browse opt cradle mdlName)
  where
    format
      | operators opt = formatOps
      | otherwise     = removeOps
    removeOps = sort . filter (isAlpha.head)
    formatOps = sort . map formatOps'
    formatOps' x@(s:_)
      | isAlpha s = x
      | otherwise = "(" ++ name ++ ")" ++ tail_
      where
        (name, tail_) = break isSpace x
    formatOps' [] = error "formatOps'"

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
    getModule =
      findModule (mkModuleName mdlName) Nothing
      `gcatch` (\e ->
                  case e of
                    -- findModule works only for package modules, moreover,
                    -- you cannot load a package module. On the other hand,
                    -- to browse a local module you need to load it first.
                    -- If CmdLineError is signalled, we assume the user
                    -- tried browsing a local module.
                    CmdLineError _ -> loadAndFind
                    _ -> throwGhcException e)
    listExports Nothing       = return []
    listExports (Just mdinfo)
      | detailed opt = processModule mdinfo
      | otherwise    = return (processExports mdinfo)
    loadAndFind = do
      setTargetFiles [mdlName]
      checkSlowAndSet
      void $ load LoadAllTargets
      findModule (mkModuleName mdlName) Nothing

processExports :: ModuleInfo -> [String]
processExports = map getOccString . modInfoExports

processModule :: ModuleInfo -> Ghc [String]
processModule minfo = mapM processName names
  where
    names = modInfoExports minfo
    processName :: Name -> Ghc String
    processName nm = do
        tyInfo <- modInfoLookupName minfo nm
        -- If nothing found, load dependent module and lookup global
        tyResult <- maybe (inOtherModule nm) (return . Just) tyInfo
        dflag <- getSessionDynFlags
        return $ fromMaybe (getOccString nm) (tyResult >>= showThing dflag)
    inOtherModule :: Name -> Ghc (Maybe TyThing)
    inOtherModule nm = getModuleInfo (nameModule nm) >> lookupGlobalName nm

showThing :: DynFlags -> TyThing -> Maybe String
showThing dflag (AnId i)     = Just $ formatType dflag varType i
showThing dflag (ADataCon d) = Just $ formatType dflag dataConRepType d
showThing _     (ATyCon t)   = unwords . toList <$> tyType t
  where
    toList t' = t' : getOccString t : map getOccString (tyConTyVars t)
showThing _     _            = Nothing

formatType :: NamedThing a => DynFlags -> (a -> Type) -> a -> String
formatType dflag f x = getOccString x ++ " :: " ++ showOutputable dflag (removeForAlls $ f x)

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
