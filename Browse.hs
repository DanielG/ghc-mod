module Browse (browseModule) where

import Control.Applicative
import Data.Char
import Data.List
import Data.Maybe (fromMaybe)
import GHC
import GHCApi
import Gap
import Name
import Outputable
import TyCon
import Type
import Types
import Var

----------------------------------------------------------------

browseModule :: Options -> String -> IO String
browseModule opt mdlName = convert opt . format <$> browse opt mdlName
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

browse :: Options -> String -> IO [String]
browse opt mdlName = withGHC $ do
    _ <- initSession0 opt
    getModule >>= getModuleInfo >>= listExports
  where
    getModule = findModule (mkModuleName mdlName) Nothing
    listExports Nothing       = return []
    listExports (Just mdinfo)
      | detailed opt = processModule mdinfo
      | otherwise    = return (processExports mdinfo)

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
        return $ fromMaybe (getOccString nm) (tyResult >>= showThing)
    inOtherModule :: Name -> Ghc (Maybe TyThing)
    inOtherModule nm = getModuleInfo (nameModule nm) >> lookupGlobalName nm

showThing :: TyThing -> Maybe String
showThing (AnId i)   = Just $ getOccString i ++ " :: " ++ showOutputable (removeForAlls $ varType i)
showThing (ATyCon t) = unwords . toList <$> tyType t
  where
    toList t' = t' : getOccString t : map getOccString (tyConTyVars t)
showThing _          = Nothing

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

showOutputable :: Outputable a => a -> String
showOutputable = unwords . lines . showDocForUser neverQualify . ppr
