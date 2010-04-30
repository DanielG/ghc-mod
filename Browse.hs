module Browse (browseModule) where

import Control.Applicative
import Data.Char
import Data.List
import Exception
import GHC
import GHC.Paths (libdir)
import Name
import Param

----------------------------------------------------------------

browseModule :: Options -> String -> IO String
browseModule opt mdlName = convert opt . validate <$> browse mdlName
  where
    validate = sort . filter (isAlpha.head)

browse :: String -> IO [String]
browse mdlName = ghandle ignore $ runGhc (Just libdir) $ do
    initSession
    maybeNamesToStrings <$> lookupModuleInfo
  where
    initSession = getSessionDynFlags >>= setSessionDynFlags
    lookupModuleInfo = findModule (mkModuleName mdlName) Nothing >>= getModuleInfo
    maybeNamesToStrings = maybe [] (map getOccString . modInfoExports)
    ignore :: SomeException -> IO [String]
    ignore _ = return []
