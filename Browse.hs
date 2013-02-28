module Browse (browseModule) where

import Control.Applicative
import Data.Char
import Data.List
import Data.Maybe (fromMaybe)
import DynFlags (getDynFlags) -- FIXME
import GHC
import GHCApi
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
    lookupModuleInfo >>= maybe (return []) (if detailed opt then processModule else return . processExports)
  where
    lookupModuleInfo = findModule (mkModuleName mdlName) Nothing >>= getModuleInfo

    processExports :: ModuleInfo -> [String]
    processExports = map getOccString . modInfoExports

    processModule :: ModuleInfo -> Ghc [String]
    processModule minfo = do
      dynFlags <- getDynFlags
      let processName :: Name -> Ghc String
          processName nm = do
              tyInfo <- modInfoLookupName minfo nm
              -- If nothing found, load dependent module and lookup global
              tyResult <- maybe inOtherModule (return . Just) tyInfo
              return $ fromMaybe name (tyResult >>= showThing dynFlags)
            where
              inOtherModule :: Ghc (Maybe TyThing)
              inOtherModule = do
                  _ <- getModuleInfo (nameModule nm) -- FIXME
                  lookupGlobalName nm
              name = getOccString nm
      mapM processName exports
        where
          exports = modInfoExports minfo

    showThing :: DynFlags -> TyThing -> Maybe String
    showThing dflags t = case t of
      AnId i -> Just $ getOccString i ++ " :: " ++ showOutputable dflags (removeForAlls $ varType i)
      ATyCon typ -> do
        tyType' <- tyType typ
        return $ unwords $ [tyType', getOccString typ] ++ map getOccString (tyConTyVars typ)
      _ -> Nothing
      where
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
    removeForAlls ty = case splitFunTy_maybe ty' of
      Nothing -> ty'
      Just (pre, ftype) -> if isPredTy pre then mkFunTy pre (dropForAlls ftype) else ty'
      where
        ty' = dropForAlls ty

showOutputable :: Outputable a => DynFlags -> a -> String
showOutputable dflags = unwords . lines . showSDocForUser dflags neverQualify . ppr
