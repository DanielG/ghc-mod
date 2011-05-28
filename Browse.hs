module Browse (browseModule) where

import Control.Applicative
import Data.Char
import Data.List
import GHC
import Name
import Types

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
      | otherwise = '(' : x ++ ")"
    formatOps' [] = error "formatOps'"

browse :: Options -> String -> IO [String]
browse opt mdlName = withGHC $ do
    initSession0 opt
    maybeNamesToStrings <$> lookupModuleInfo
  where
    lookupModuleInfo = findModule (mkModuleName mdlName) Nothing >>= getModuleInfo
    maybeNamesToStrings = maybe [] (map getOccString . modInfoExports)
