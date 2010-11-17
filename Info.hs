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
import Control.Exception

type Expression = String
type ModuleString = String

----------------------------------------------------------------

typeExpr :: Options -> ModuleString -> Expression -> FilePath -> IO String
typeExpr _ modstr expr file = (++ "\n") <$> typeOf file modstr expr

typeOf :: FilePath -> ModuleString-> Expression -> IO String
typeOf fileName modstr expr = withGHC $ valid `gcatch` invalid
  where
    valid = makeTypeOf LoadAllTargets
    invalid = constE invalid0
    invalid0 = makeTypeOf $ LoadDependenciesOf (mkModuleName modstr)
    makeTypeOf x = do
        initSession ["-w"]
        setTargetFile fileName
        loadWithLogger (\_ -> return ()) x
        setContextFromTarget
        pretty <$> exprType expr
    pretty = showSDocForUser neverQualify . pprTypeForUser False

----------------------------------------------------------------

infoExpr :: Options -> ModuleString -> Expression -> FilePath -> IO String
infoExpr _ modstr expr file = (++ "\n") <$> info file modstr expr

info :: FilePath -> ModuleString -> FilePath -> IO String
info fileName modstr expr = withGHC $ valid `gcatch` invalid
  where
    valid = makeInfo LoadAllTargets
    invalid = constE invalid0
    invalid0 = makeInfo $ LoadDependenciesOf (mkModuleName modstr)
    makeInfo x = do
        initSession ["-w"]
        setTargetFile fileName
        loadWithLogger (\_ -> return ()) x
        setContextFromTarget
        infoThing expr
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
    mdls <- mapM toModule ms
    setContext (catMaybes mdls) []
 where
   toModule ms = lookupMod `gcatch` nothing
     where
       lookupMod = lookupModule (ms_mod_name ms) Nothing >>= return . Just
       nothing = constE $ return Nothing

----------------------------------------------------------------

constE :: a -> (SomeException -> a)
constE func = \_ -> func
