module Info where

import Control.Applicative
import GHC
import Outputable
import PprTyThing
import Types

typeExpr :: Options -> String -> String -> IO String
typeExpr _ expr file = (++ "\n") <$> typeOf file expr

typeOf :: String -> String -> IO String
typeOf fileName expr = withGHC $ do
    initSession []
    setTargetFile fileName
    load LoadAllTargets
    setContextFromTarget
    pretty <$> exprType expr
  where
    setContextFromTarget = do
        ms <- depanal [] False
        mdl <- findModule (ms_mod_name (head ms)) Nothing
        setContext [mdl] []
    pretty = showSDocForUser neverQualify . pprTypeForUser False
