module Browse (browseModule) where

import Control.Applicative
import Data.Char
import Data.List
import GHC
import Name
import Types

----------------------------------------------------------------

browseModule :: Options -> String -> IO String
browseModule opt mdlName = convert opt . validate <$> browse mdlName
  where
    validate = sort . filter (isAlpha.head)

browse :: String -> IO [String]
browse mdlName = withGHC $ do
    initSession0
    maybeNamesToStrings <$> lookupModuleInfo
  where
    lookupModuleInfo = findModule (mkModuleName mdlName) Nothing >>= getModuleInfo
    maybeNamesToStrings = maybe [] (map getOccString . modInfoExports)
