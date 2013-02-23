module Browse (browseModule) where

import Control.Applicative
import Data.Char
import Data.List
import Data.Maybe (fromMaybe)
import GHC
import GHCApi
import DynFlags (getDynFlags)
import Name
import Types
import Outputable
import Var
import TyCon

----------------------------------------------------------------

browseModule :: Options -> String -> IO String
browseModule opt mdlName = (convert opt . format) <$> browse opt mdlName
  where
    format
      | operators opt = formatOps
      | otherwise     = removeOps
    removeOps = sort . filter (isAlpha.head)
    formatOps = sort . map formatOps'
    formatOps' x@(s:_)
      | isAlpha s = x
      | otherwise = '(' : x ++ ")"
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
      let
        processName :: Name -> Ghc String
        processName nm = do
          tyInfo <- modInfoLookupName minfo nm
          return $ fromMaybe (getOccString nm) (tyInfo >>= showThing dynFlags)
      mapM processName exports
      where
        exports = modInfoExports minfo

    showThing :: DynFlags -> TyThing -> Maybe String
    showThing dflags t = case t of
      (AnId i) -> Just $ getOccString i ++ " :: " ++ showOutputable dflags (snd $ splitForAllTys $ varType i)
      (ATyCon t) -> do
        tyType' <- tyType t
        return $ intercalate " " $ [tyType', getOccString t] ++ map getOccString (tyConTyVars t)
      _ -> Nothing
      where
        tyType :: TyCon -> Maybe String
        tyType t
          | isAlgTyCon t && not (isNewTyCon t) && not (isClassTyCon t) = Just "data"
          | isNewTyCon t = Just "newtype"
          | isClassTyCon t = Just "class"
          | isSynTyCon t = Just "type"
          | otherwise = Nothing

showOutputable :: Outputable a => DynFlags -> a -> String
showOutputable dflags = unwords . lines . showSDocForUser dflags neverQualify . ppr
