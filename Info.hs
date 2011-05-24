module Info where

import Control.Applicative hiding (empty)
import Control.Monad
import Data.Maybe
import GHC
import Outputable
import PprTyThing
import Types
import NameSet
import HscTypes
import Data.List
import Control.Exception
import StringBuffer
import System.Time

type Expression = String
type ModuleString = String

----------------------------------------------------------------

typeExpr :: Options -> ModuleString -> Expression -> FilePath -> IO String
typeExpr _ modstr expr file = (++ "\n") <$> typeOf file modstr expr

typeOf :: FilePath -> ModuleString -> Expression -> IO String
typeOf fileName modstr expr = inModuleContext fileName modstr exprToType
  where
    exprToType = pretty <$> exprType expr
    pretty = showSDocForUser neverQualify . pprTypeForUser False

----------------------------------------------------------------

infoExpr :: Options -> ModuleString -> Expression -> FilePath -> IO String
infoExpr _ modstr expr file = (++ "\n") <$> info file modstr expr

info :: FilePath -> ModuleString -> FilePath -> IO String
info fileName modstr expr = inModuleContext fileName modstr exprToInfo
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

inModuleContext :: FilePath -> ModuleString -> Ghc String -> IO String
inModuleContext fileName modstr action = withGHC valid
  where
    valid = do
        initSession ["-w"] Nothing
        setTargetFile fileName
        loadWithLogger (\_ -> return ()) LoadAllTargets
        mif setContextFromTarget action invalid
    invalid = do
        initSession ["-w"] Nothing
        setTargetBuffer
        loadWithLogger defaultWarnErrLogger LoadAllTargets
        mif setContextFromTarget action (return errorMessage)
    setTargetBuffer = do
        modgraph <- depanal [mkModuleName modstr] True
        let imports = concatMap (map (showSDoc . ppr . unLoc)) $
                      map ms_imps modgraph ++ map ms_srcimps modgraph
            moddef = "module " ++ sanitize modstr ++ " where"
            header = moddef : imports
        importsBuf <- liftIO . stringToStringBuffer . unlines $ header
        clkTime <- liftIO getClockTime
        setTargets [Target (TargetModule $ mkModuleName modstr) True (Just (importsBuf, clkTime))]
    mif m t e = m >>= \ok -> if ok then t else e
    sanitize = fromMaybe "SomeModule" . listToMaybe . words
    errorMessage = "Couldn't determine type"

setContextFromTarget :: Ghc Bool
setContextFromTarget = do
    ms <- depanal [] False
    top <- map ms_mod <$> filterM isTop ms
    setContext top []
    return (top /= [])
  where
    isTop ms = lookupMod `gcatch` returnFalse
      where
        lookupMod = lookupModule (ms_mod_name ms) Nothing >> return True
        returnFalse = constE $ return False

----------------------------------------------------------------

constE :: a -> (SomeException -> a)
constE func = \_ -> func
