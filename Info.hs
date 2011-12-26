{-# LANGUAGE CPP #-}

module Info where

import Cabal
import Control.Applicative hiding (empty)
import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import GHC
import HscTypes
import NameSet
import Outputable
import PprTyThing
import StringBuffer
import System.Time
import Types

#if __GLASGOW_HASKELL__ >= 702
import CoreMonad
#endif

type Expression = String
type ModuleString = String

----------------------------------------------------------------

typeExpr :: Options -> ModuleString -> Expression -> FilePath -> IO String
typeExpr opt modstr expr file = (++ "\n") <$> typeOf opt file modstr expr

typeOf :: Options -> FilePath -> ModuleString -> Expression -> IO String
typeOf opt fileName modstr expr = inModuleContext opt fileName modstr exprToType
  where
    exprToType = pretty <$> exprType expr
    pretty = showSDocForUser neverQualify . pprTypeForUser False

----------------------------------------------------------------

infoExpr :: Options -> ModuleString -> Expression -> FilePath -> IO String
infoExpr opt modstr expr file = (++ "\n") <$> info opt file modstr expr

info :: Options -> FilePath -> ModuleString -> FilePath -> IO String
info opt fileName modstr expr = inModuleContext opt fileName modstr exprToInfo
  where
    exprToInfo = infoThing expr

----------------------------------------------------------------
-- from ghc/InteractiveUI.hs

infoThing :: String -> Ghc String
infoThing str = do
    names <- parseName str
    mb_stuffs <- mapM getInfo names
    let filtered = filterOutChildren (\(t,_f,_i) -> t) (catMaybes mb_stuffs)
    unqual <- getPrintUnqual
    return $ showSDocForUser unqual $ vcat (intersperse (text "") $ map (pprInfo False) filtered)

filterOutChildren :: (a -> TyThing) -> [a] -> [a]
filterOutChildren get_thing xs
  = [x | x <- xs, not (getName (get_thing x) `elemNameSet` implicits)]
  where
      implicits = mkNameSet [getName t | x <- xs, t <- implicitTyThings (get_thing x)]

pprInfo :: PrintExplicitForalls -> (TyThing, Fixity, [Instance]) -> SDoc
pprInfo pefas (thing, fixity, insts)
    = pprTyThingInContextLoc pefas thing
   $$ show_fixity fixity
   $$ vcat (map pprInstance insts)
  where
    show_fixity fix
        | fix == defaultFixity = empty
        | otherwise            = ppr fix <+> ppr (getName thing)

----------------------------------------------------------------

inModuleContext :: Options -> FilePath -> ModuleString -> Ghc String -> IO String
inModuleContext opt fileName modstr action = withGHC valid
  where
    valid = do
        (file,_) <- initializeGHC opt fileName ["-w"] False
        setTargetFile file
        load LoadAllTargets
        mif setContextFromTarget action invalid
    invalid = do
        initializeGHC opt fileName ["-w"] False
        setTargetBuffer
        load LoadAllTargets
        mif setContextFromTarget action (return errorMessage)
    setTargetBuffer = do
        modgraph <- depanal [mkModuleName modstr] True
        let imports = concatMap (map (showSDoc . ppr . unLoc)) $
                      map ms_imps modgraph ++ map ms_srcimps modgraph
            moddef = "module " ++ sanitize modstr ++ " where"
            header = moddef : imports
#if __GLASGOW_HASKELL__ >= 702
            importsBuf = stringToStringBuffer . unlines $ header
#else
        importsBuf <- liftIO . stringToStringBuffer . unlines $ header
#endif
        clkTime <- liftIO getClockTime
        setTargets [Target (TargetModule $ mkModuleName modstr) True (Just (importsBuf, clkTime))]
    mif m t e = m >>= \ok -> if ok then t else e
    sanitize = fromMaybe "SomeModule" . listToMaybe . words
    errorMessage = "Couldn't determine type"

setContextFromTarget :: Ghc Bool
setContextFromTarget = do
    ms <- depanal [] False

#if __GLASGOW_HASKELL__ >= 704
    top <- map (IIModule . ms_mod) <$> filterM isTop ms
    setContext top
#else
    top <- map ms_mod <$> filterM isTop ms
    setContext top []
#endif
    return (not . null $ top)
  where
    isTop ms = lookupMod `gcatch` returnFalse
      where
        lookupMod = lookupModule (ms_mod_name ms) Nothing >> return True
        returnFalse = constE $ return False

----------------------------------------------------------------

constE :: a -> (SomeException -> a)
constE func = \_ -> func
