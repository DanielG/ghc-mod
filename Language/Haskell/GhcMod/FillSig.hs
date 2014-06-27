{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Language.Haskell.GhcMod.FillSig (
    fillSig
  , sig
  ) where

import Data.Char (isSymbol)
import Data.List (find, intercalate)
import Exception (ghandle, SomeException(..))
import GHC (Ghc, Id, ParsedModule(..), TypecheckedModule(..), DynFlags, SrcSpan, Type, GenLocated(L))
import qualified GHC as G
import Language.Haskell.GhcMod.GHCApi
import qualified Language.Haskell.GhcMod.Gap as Gap
import Language.Haskell.GhcMod.SrcUtils
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Convert
import MonadUtils (liftIO)
import Outputable (PprStyle)
import qualified Type as Ty
import qualified HsBinds as Ty
import qualified Class as Ty
import OccName (occName)
import qualified Language.Haskell.Exts.Annotated as HE

----------------------------------------------------------------
-- INTIAL CODE FROM FUNCTION OR INSTANCE SIGNATURE
----------------------------------------------------------------

-- Possible signatures we can find: function or instance
data SigInfo = Signature SrcSpan [G.RdrName] (G.HsType G.RdrName)
             | InstanceDecl SrcSpan G.Class

-- Signature for fallback operation via haskell-src-exts
data HESigInfo = HESignature HE.SrcSpan [HE.Name HE.SrcSpanInfo] (HE.Type HE.SrcSpanInfo) 

-- | Create a initial body from a signature.
fillSig :: Options
        -> Cradle
        -> FilePath     -- ^ A target file.
        -> Int          -- ^ Line number.
        -> Int          -- ^ Column number.
        -> IO String
fillSig opt cradle file lineNo colNo = withGHC' $ do
    initializeFlagsWithCradle opt cradle
    sig opt file lineNo colNo

-- | Create a initial body from a signature.
sig :: Options
    -> FilePath     -- ^ A target file.
    -> Int          -- ^ Line number.
    -> Int          -- ^ Column number.
    -> Ghc String
sig opt file lineNo colNo = ghandle handler body
  where
    body = inModuleContext file $ \dflag style -> do
        modSum <- Gap.fileModSummary file
        whenFound opt (getSignature modSum lineNo colNo) $ \s -> case s of
          Signature loc names ty ->
            ("function", fourInts loc, map (initialBody dflag style ty) names)
          InstanceDecl loc cls -> do
             ("instance", fourInts loc, map (\x -> initialBody dflag style (G.idType x) x)
                                            (Ty.classMethods cls))
            
    handler (SomeException _) = do
      -- Code cannot be parsed by ghc module
      -- Fallback: try to get information via haskell-src-exts
      whenFound opt (getSignatureFromHE file lineNo colNo) $
        \(HESignature loc names ty) -> 
           ("function", fourIntsHE loc, map (initialBody undefined undefined ty) names)

----------------------------------------------------------------
-- a. Code for getting the information

-- Get signature from ghc parsing and typechecking
getSignature :: G.ModSummary -> Int -> Int -> Ghc (Maybe SigInfo)
getSignature modSum lineNo colNo = do
    p@ParsedModule{pm_parsed_source = ps} <- G.parseModule modSum
    -- Inspect the parse tree to find the signature
    case listifyParsedSpans ps (lineNo, colNo) :: [G.LHsDecl G.RdrName] of
      [L loc (G.SigD (Ty.TypeSig names (L _ ty)))] ->
        -- We found a type signature
        return $ Just $ Signature loc (map G.unLoc names) ty
      [L _ (G.InstD _)] -> do
        -- We found an instance declaration
        TypecheckedModule{tm_renamed_source = Just tcs
                         ,tm_checked_module_info = minfo} <- G.typecheckModule p
        case listifyRenamedSpans tcs (lineNo, colNo) :: [G.LInstDecl G.Name] of
          -- Instance declarations of sort 'instance F (G a)'
          [L loc (G.ClsInstD (G.ClsInstDecl {G.cid_poly_ty =
            (L _ (G.HsForAllTy _ _ _ (L _ (G.HsAppTy (L _ (G.HsTyVar clsName)) _))))}))] ->
               obtainClassInfo minfo clsName loc
          -- Instance declarations of sort 'instance F G' (no variables)
          [L loc (G.ClsInstD (G.ClsInstDecl {G.cid_poly_ty =
            (L _ (G.HsAppTy (L _ (G.HsTyVar clsName)) _))}))] ->
               obtainClassInfo minfo clsName loc
          _ -> return Nothing
      _ -> return Nothing
  where obtainClassInfo :: G.ModuleInfo -> G.Name -> SrcSpan -> Ghc (Maybe SigInfo)
        obtainClassInfo minfo clsName loc = do
          tyThing <- G.modInfoLookupName minfo clsName
          return $ do Ty.ATyCon clsCon <- tyThing  -- In Maybe
                      cls <- G.tyConClass_maybe clsCon
                      return $ InstanceDecl loc cls

-- Get signature from haskell-src-exts
getSignatureFromHE :: FilePath -> Int -> Int -> Ghc (Maybe HESigInfo)
getSignatureFromHE file lineNo colNo = do
  presult <- liftIO $ HE.parseFile file
  return $ case presult of
             HE.ParseOk (HE.Module _ _ _ _ mdecls) -> do
               HE.TypeSig (HE.SrcSpanInfo s _) names ty <- find (typeSigInRangeHE lineNo colNo) mdecls
               return $ HESignature s names ty
             _ -> Nothing

----------------------------------------------------------------
-- b. Code for generating initial code

-- A list of function arguments, and whether they are functions or normal arguments
-- is built from either a function signature or an instance signature
data FnArg = FnArgFunction | FnArgNormal

initialBody :: FnArgsInfo ty name => DynFlags -> PprStyle -> ty -> name -> String
initialBody dflag style ty name = initialBody' (getFnName dflag style name) (getFnArgs ty)

initialBody' :: String -> [FnArg] -> String
initialBody' fname args =
  case initialBodyArgs args infiniteVars infiniteFns of
    []      -> fname
    arglist -> if isSymbolName fname
               then (head arglist) ++ " " ++ fname ++ " " ++ (intercalate " " (tail arglist))
               else fname ++ " " ++ (intercalate " " arglist)
  ++ " = " ++ (if isSymbolName fname then "" else '_':fname) ++ "_body"

initialBodyArgs :: [FnArg] -> [String] -> [String] -> [String]
initialBodyArgs [] _ _ = []
initialBodyArgs (FnArgFunction:xs) vs (f:fs) = f : initialBodyArgs xs vs fs
initialBodyArgs (FnArgNormal:xs)   (v:vs) fs = v : initialBodyArgs xs vs fs
initialBodyArgs _                  _      _  = error "This should never happen" -- Lists are infinite

-- Getting the initial body of function and instances differ
-- This is because for functions we only use the parsed file
-- (so the full file doesn't have to be type correct)
-- but for instances we need to get information about the class

class FnArgsInfo ty name | ty -> name, name -> ty where
  getFnName :: DynFlags -> PprStyle -> name -> String
  getFnArgs :: ty -> [FnArg]

instance FnArgsInfo (G.HsType G.RdrName) (G.RdrName) where
  getFnName dflag style name = showOccName dflag style $ occName name
  getFnArgs (G.HsForAllTy _ _ _ (L _ iTy))  = getFnArgs iTy
  getFnArgs (G.HsParTy (L _ iTy))           = getFnArgs iTy
  getFnArgs (G.HsFunTy (L _ lTy) (L _ rTy)) = (if fnarg lTy then FnArgFunction else FnArgNormal):getFnArgs rTy
    where fnarg = \ty -> case ty of
                           (G.HsForAllTy _ _ _ (L _ iTy)) -> fnarg iTy
                           (G.HsParTy (L _ iTy))          -> fnarg iTy
                           (G.HsFunTy _ _)                -> True
                           _                              -> False
  getFnArgs _ = []

instance FnArgsInfo (HE.Type HE.SrcSpanInfo) (HE.Name HE.SrcSpanInfo) where
  getFnName _ _ (HE.Ident  _ s) = s
  getFnName _ _ (HE.Symbol _ s) = s
  getFnArgs (HE.TyForall _ _ _ iTy) = getFnArgs iTy
  getFnArgs (HE.TyParen _ iTy)      = getFnArgs iTy
  getFnArgs (HE.TyFun _ lTy rTy)    = (if fnarg lTy then FnArgFunction else FnArgNormal):getFnArgs rTy
    where fnarg = \ty -> case ty of
                           (HE.TyForall _ _ _ iTy) -> fnarg iTy
                           (HE.TyParen _ iTy)      -> fnarg iTy
                           (HE.TyFun _ _ _)        -> True
                           _                       -> False
  getFnArgs _ = []

instance FnArgsInfo Type Id where
  getFnName dflag style method = showOccName dflag style $ G.getOccName method
  getFnArgs = getFnArgs' . Ty.dropForAlls
    where getFnArgs' ty | Just (lTy,rTy) <- Ty.splitFunTy_maybe ty =
            maybe (if Ty.isPredTy lTy then getFnArgs' rTy else FnArgNormal:getFnArgs' rTy)
                  (\_ -> FnArgFunction:getFnArgs' rTy)
                  $ Ty.splitFunTy_maybe lTy
          getFnArgs' ty | Just (_,iTy) <- Ty.splitForAllTy_maybe ty = getFnArgs' iTy
          getFnArgs' _ = []

-- Infinite supply of variable and function variable names
infiniteVars, infiniteFns :: [String]
infiniteVars = infiniteSupply ["x","y","z","t","u","v","w"]
infiniteFns  = infiniteSupply ["f","g","h"]
infiniteSupply :: [String] -> [String]
infiniteSupply initialSupply = initialSupply ++ concatMap (\n -> map (\v -> v ++ show n) initialSupply) ([1 .. ] :: [Integer])

-- Check whether a String is a symbol name
isSymbolName :: String -> Bool
isSymbolName (c:_) = c `elem` "!#$%&*+./<=>?@\\^|-~" || isSymbol c
isSymbolName []    = error "This should never happen"
