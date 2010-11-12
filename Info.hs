module Info where

import Control.Applicative
import GHC
import Outputable
import PprTyThing
import Types

infoExpr :: Options -> String -> String -> IO String
infoExpr _ expr file = (++ "\n") <$> info file expr

info :: String -> String -> IO String
info fileName expr = withGHC $ do
    initSession []
    setTargetFile fileName
    load LoadAllTargets
    setContextFromTarget
    pretty <$> exprType expr
  where
    setContextFromTarget = do
        [mdlsum,_] <- depanal [] False
        mdl <- findModule (ms_mod_name mdlsum) Nothing
        setContext [mdl] []
    pretty = showSDocForUser neverQualify . pprTypeForUser False
