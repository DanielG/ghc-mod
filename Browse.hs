module Browse (browseModule) where

import Control.Applicative
import Data.Char
import Data.List
import Data.Maybe (fromMaybe, catMaybes)
import GHC
import GHCApi
import DynFlags (getDynFlags)
import Name
import Types
import Outputable
import Var
import TyCon

import MonadUtils (liftIO)
import Debug.Trace

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
    liftIO $ traceIO $ "Module name is " ++ mdlName
    _ <- initSession0 opt
    liftIO $ traceIO "Session inited"
    lookupModuleInfo >>= maybe (return []) (if detailed opt then processModule else return . processExports)
  where
    lookupModuleInfo = findModule (mkModuleName mdlName) Nothing >>= getModuleInfo

    processExports :: ModuleInfo -> [String]
    processExports = map getOccString . modInfoExports

    processModule :: ModuleInfo -> Ghc [String]
    processModule minfo = do
      dynFlags <- getDynFlags
      let
        processName :: Name -> Ghc (Maybe String)
        processName nm = fmap (\thing -> thing >>= showThing dynFlags) $ modInfoLookupName minfo nm
      fmap catMaybes $ mapM processName exports
      where
        exports = modInfoExports minfo

    showThing :: DynFlags -> TyThing -> Maybe String
    showThing dflags (AnId i) = Just $ getOccString i ++ " :: " ++ showOutputable dflags (varType i)
    showThing dflags (ATyCon t) = Just $ intercalate " " $ [tyType t, getOccString t] ++ map getOccString (tyConTyVars t) where
      tyType :: TyCon -> String
      tyType t
        | isAlgTyCon t && not (isNewTyCon t) = "data"
        | isNewTyCon t = "newtype"
        | isClassTyCon t = "class"
        | isSynTyCon t = "type"
        | otherwise = "WTF"
    showThing dflags _ = Nothing

showOutputable :: Outputable a => DynFlags -> a -> String
showOutputable dflags = showSDocForUser dflags neverQualify . ppr
