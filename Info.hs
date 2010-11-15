module Info where

import Control.Applicative hiding (empty)
import Data.Maybe
import GHC
import Outputable
import PprTyThing
import Types
import NameSet
import HscTypes
import Data.List

----------------------------------------------------------------

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
    pretty = showSDocForUser neverQualify . pprTypeForUser False

----------------------------------------------------------------

infoExpr :: Options -> String -> String -> IO String
infoExpr _ expr file = (++ "\n") <$> info file expr

info :: String -> String -> IO String
info fileName expr = withGHC $ do
    initSession []
    setTargetFile fileName
    load LoadAllTargets
    setContextFromTarget
    infoThing expr
  where
    -- ghc/InteractiveUI.hs
    infoThing str = do
        names <- parseName str
        mb_stuffs <- mapM getInfo names
        let filtered = filterOutChildren (\(t,_f,_i) -> t) (catMaybes mb_stuffs)
        unqual <- getPrintUnqual
        return $ showSDocForUser unqual $ vcat (intersperse (text "") $ map (pprInfo False) filtered)

-- ghc/InteractiveUI.hs
filterOutChildren :: (a -> TyThing) -> [a] -> [a]
filterOutChildren get_thing xs
  = [x | x <- xs, not (getName (get_thing x) `elemNameSet` implicits)]
  where
      implicits = mkNameSet [getName t | x <- xs, t <- implicitTyThings (get_thing x)]

pprInfo :: PrintExplicitForalls -> (TyThing, Fixity, [GHC.Instance]) -> SDoc
pprInfo pefas (thing, fixity, insts)
  =  pprTyThingInContextLoc pefas thing
  $$ show_fixity fixity
  $$ vcat (map pprInstance insts)
  where
    show_fixity fix
        | fix == defaultFixity = empty
        | otherwise            = ppr fix <+> ppr (getName thing)

----------------------------------------------------------------

setContextFromTarget :: Ghc ()
setContextFromTarget = do
    ms <- depanal [] False
    mdl <- findModule (ms_mod_name (head ms)) Nothing
    setContext [mdl] []
