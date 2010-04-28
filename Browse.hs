module Browse (browseModule) where

import Control.Applicative
import Data.Char
import Data.List
import DynFlags
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
browse mdlName = withGHCAPI (maybeNamesToStrings <$> lookupModuleInfo)

  where
    lookupModuleInfo = findModule (mkModuleName mdlName) Nothing >>= getModuleInfo
    maybeNamesToStrings = maybe [] (map getOccString . modInfoExports)

withGHCAPI :: Ghc a -> IO a
withGHCAPI body = defaultErrorHandler defaultDynFlags $
    runGhc (Just libdir) $ do
        getSessionDynFlags >>= setSessionDynFlags
        body
