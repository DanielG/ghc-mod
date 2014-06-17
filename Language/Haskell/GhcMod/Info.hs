{-# LANGUAGE TupleSections, FlexibleInstances, Rank2Types, LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Haskell.GhcMod.Info (
    infoExpr
  , info
  , typeExpr
  , types
  , splitVar
  , splits
  , fillSig
  , sig
  ) where

import Control.Applicative ((<$>))
import CoreMonad (liftIO)
import CoreUtils (exprType)
import Data.Char (isSymbol)
import Data.Function (on)
import Data.Generics
import Data.List (find, intercalate, sortBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord as O
import Exception (ghandle, SomeException(..))
import GHC (Ghc, LHsBind, LHsExpr, LPat, Id, ParsedModule(..), TypecheckedModule(..), DynFlags, SrcSpan, Type, Located, ParsedSource, RenamedSource, TypecheckedSource, GenLocated(L))
import qualified GHC as G
import GHC.SYB.Utils (Stage(..), everythingStaged, showData)
import Language.Haskell.GhcMod.Doc (showPage, showOneLine, getStyle)
import Language.Haskell.GhcMod.GHCApi
import Language.Haskell.GhcMod.Gap (HasType(..), setWarnTypedHoles, setDeferTypeErrors)
import qualified Language.Haskell.GhcMod.Gap as Gap
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Convert
import Outputable (PprStyle)
import TcHsSyn (hsPatType)
import qualified Type as Ty
import qualified TyCon as Ty
import qualified DataCon as Ty
import qualified HsBinds as Ty
import qualified Class as Ty
import OccName (OccName, occName)

----------------------------------------------------------------

-- | Obtaining information of a target expression. (GHCi's info:)
infoExpr :: Options
         -> Cradle
         -> FilePath     -- ^ A target file.
         -> Expression   -- ^ A Haskell expression.
         -> IO String
infoExpr opt cradle file expr = withGHC' $ do
    initializeFlagsWithCradle opt cradle
    info opt file expr

-- | Obtaining information of a target expression. (GHCi's info:)
info :: Options
     -> FilePath     -- ^ A target file.
     -> Expression   -- ^ A Haskell expression.
     -> Ghc String
info opt file expr = convert opt <$> ghandle handler body
  where
    body = inModuleContext file $ \dflag style -> do
        sdoc <- Gap.infoThing expr
        return $ showPage dflag style sdoc
    handler (SomeException _) = return "Cannot show info"

----------------------------------------------------------------

instance HasType (LHsExpr Id) where
    getType tcm e = do
        hs_env <- G.getSession
        mbe <- liftIO $ Gap.deSugar tcm e hs_env
        return $ (G.getLoc e, ) <$> CoreUtils.exprType <$> mbe

instance HasType (LPat Id) where
    getType _ (G.L spn pat) = return $ Just (spn, hsPatType pat)

----------------------------------------------------------------

-- | Obtaining type of a target expression. (GHCi's type:)
typeExpr :: Options
         -> Cradle
         -> FilePath     -- ^ A target file.
         -> Int          -- ^ Line number.
         -> Int          -- ^ Column number.
         -> IO String
typeExpr opt cradle file lineNo colNo = withGHC' $ do
    initializeFlagsWithCradle opt cradle
    types opt file lineNo colNo

-- | Obtaining type of a target expression. (GHCi's type:)
types :: Options
      -> FilePath     -- ^ A target file.
      -> Int          -- ^ Line number.
      -> Int          -- ^ Column number.
      -> Ghc String
types opt file lineNo colNo = convert opt <$> ghandle handler body
  where
    body = inModuleContext file $ \dflag style -> do
        modSum <- Gap.fileModSummary file
        srcSpanTypes <- getSrcSpanType modSum lineNo colNo
        return $ map (toTup dflag style) $ sortBy (cmp `on` fst) srcSpanTypes
    handler (SomeException _) = return []

getSrcSpanType :: G.ModSummary -> Int -> Int -> Ghc [(SrcSpan, Type)]
getSrcSpanType modSum lineNo colNo = do
    p <- G.parseModule modSum
    tcm@TypecheckedModule{tm_typechecked_source = tcs} <- G.typecheckModule p
    let bs = listifySpans tcs (lineNo, colNo) :: [LHsBind Id]
        es = listifySpans tcs (lineNo, colNo) :: [LHsExpr Id]
        ps = listifySpans tcs (lineNo, colNo) :: [LPat Id]
    bts <- mapM (getType tcm) bs
    ets <- mapM (getType tcm) es
    pts <- mapM (getType tcm) ps
    return $ catMaybes $ concat [ets, bts, pts]

listifySpans :: Typeable a => TypecheckedSource -> (Int, Int) -> [Located a]
listifySpans tcs lc = listifyStaged TypeChecker p tcs
  where
    p (L spn _) = G.isGoodSrcSpan spn && spn `G.spans` lc

listifyParsedSpans :: Typeable a => ParsedSource -> (Int, Int) -> [Located a]
listifyParsedSpans pcs lc = listifyStaged Parser p pcs
  where
    p (L spn _) = G.isGoodSrcSpan spn && spn `G.spans` lc

listifyRenamedSpans :: Typeable a => RenamedSource -> (Int, Int) -> [Located a]
listifyRenamedSpans pcs lc = listifyStaged Renamer p pcs
  where
    p (L spn _) = G.isGoodSrcSpan spn && spn `G.spans` lc

listifyStaged :: Typeable r => Stage -> (r -> Bool) -> GenericQ [r]
listifyStaged s p = everythingStaged s (++) [] ([] `mkQ` (\x -> [x | p x]))

cmp :: SrcSpan -> SrcSpan -> Ordering
cmp a b
  | a `G.isSubspanOf` b = O.LT
  | b `G.isSubspanOf` a = O.GT
  | otherwise           = O.EQ

toTup :: DynFlags -> PprStyle -> (SrcSpan, Type) -> ((Int,Int,Int,Int),String)
toTup dflag style (spn, typ) = (fourInts spn, pretty dflag style typ)

fourInts :: SrcSpan -> (Int,Int,Int,Int)
fourInts = fromMaybe (0,0,0,0) . Gap.getSrcSpan

pretty :: DynFlags -> PprStyle -> Type -> String
pretty dflag style = showOneLine dflag style . Gap.typeForUser

----------------------------------------------------------------

inModuleContext :: FilePath -> (DynFlags -> PprStyle -> Ghc a) -> Ghc a
inModuleContext file action =
    withDynFlags (setWarnTypedHoles . setDeferTypeErrors . setNoWaringFlags) $ do
    setTargetFiles [file]
    Gap.withContext $ do
        dflag <- G.getSessionDynFlags
        style <- getStyle
        action dflag style

----------------------------------------------------------------

data SplitInfo = SplitInfo G.Name (SrcSpan, Type) (SrcSpan, Type)

-- | Splitting a variable in a equation.
splitVar :: Options
         -> Cradle
         -> FilePath     -- ^ A target file.
         -> Int          -- ^ Line number.
         -> Int          -- ^ Column number.
         -> IO String
splitVar opt cradle file lineNo colNo = withGHC' $ do
    initializeFlagsWithCradle opt cradle
    splits opt file lineNo colNo

-- | Splitting a variable in a equation.
splits :: Options
       -> FilePath     -- ^ A target file.
       -> Int          -- ^ Line number.
       -> Int          -- ^ Column number.
       -> Ghc String
splits opt file lineNo colNo = ghandle handler body
  where
    body = inModuleContext file $ \dflag style -> do
        modSum <- Gap.fileModSummary file
        splitInfo <- getSrcSpanTypeForSplit modSum lineNo colNo
        case splitInfo of
          Nothing -> return ""
          Just (SplitInfo varName var@(_,varT) eq) -> do
            return $ convert opt $ ( toTup dflag style var
                                   , toTup dflag style eq
                                   , getTyCons dflag style varName varT)
    handler (SomeException _) = return []

getSrcSpanTypeForSplit :: G.ModSummary -> Int -> Int -> Ghc (Maybe SplitInfo)
getSrcSpanTypeForSplit modSum lineNo colNo = do
    p <- G.parseModule modSum
    tcm@TypecheckedModule{tm_typechecked_source = tcs} <- G.typecheckModule p
    let bs:_ = listifySpans tcs (lineNo, colNo) :: [LHsBind Id]
        ps = find isPatternVar $ listifySpans tcs (lineNo, colNo) :: Maybe (LPat Id)
    case ps of
      Nothing  -> return Nothing
      Just ps' -> do bts <- getType tcm bs
                     pts <- getType tcm ps'
                     case (bts, pts) of
                       (Just bI, Just pI) -> return $ Just (SplitInfo (getPatternVarName ps') pI bI)
                       _                  -> return Nothing

isPatternVar :: LPat Id -> Bool
isPatternVar (L _ (G.VarPat _)) = True
isPatternVar _                  = False

getPatternVarName :: LPat Id -> G.Name
getPatternVarName (L _ (G.VarPat vName)) = G.getName vName
getPatternVarName _                      = error "This should never happend"

getTyCons :: DynFlags -> PprStyle -> G.Name -> G.Type ->  [String]
getTyCons dflag style name ty | Just (tyCon, _) <- Ty.splitTyConApp_maybe ty =
  let name' = showName dflag style name  -- Convert name to string
  in getTyCon dflag style name' tyCon
getTyCons dflag style name _ = [showName dflag style name]

-- Write cases for one type
getTyCon :: DynFlags -> PprStyle -> String -> Ty.TyCon -> [String]
-- 1. Non-matcheable type constructors
getTyCon _ _ name tyCon | isNotMatcheableTyCon tyCon = [name]
-- 2. Special cases
-- 2.1. Tuples
getTyCon _ _ name tyCon | Ty.isTupleTyCon tyCon =
  let [uniqueDataCon] = Ty.tyConDataCons tyCon
      tupleArity = Ty.dataConSourceArity uniqueDataCon
      -- Deal with both boxed and unboxed tuples
      isUnboxed = Ty.isUnboxedTupleTyCon tyCon
      startSign = if isUnboxed then "(#" else "("
      endSign   = if isUnboxed then "#)" else ")"
  in [ startSign ++ intercalate "," (map (\n -> name ++ show n) [1 .. tupleArity]) ++ endSign ]
-- 3. General case
getTyCon dflag style name tyCon = map (getDataCon dflag style name) (Ty.tyConDataCons tyCon)

-- These type constructors should not be matched against
isNotMatcheableTyCon :: Ty.TyCon -> Bool
isNotMatcheableTyCon ty = Ty.isPrimTyCon ty  -- Primitive types, such as Int#
                       || Ty.isFunTyCon ty   -- Function types

-- Write case for one constructor
getDataCon :: DynFlags -> PprStyle -> String -> Ty.DataCon -> String
-- 1. Infix constructors
getDataCon dflag style vName dcon | Ty.dataConIsInfix dcon =
  let dName  = showName dflag style $ Ty.dataConName dcon
  in case Ty.dataConSourceArity dcon of
       0 -> dName
       1 -> vName ++ dName
       n -> if dName == ":" -- Special case for lists
            then vName ++ ":" ++ vName ++ "s"
            else newVar vName 1 ++ " " ++ dName ++ " " ++ newVars vName 2 (n-1)
-- 2. Non-record, non-infix syntax
getDataCon dflag style vName dcon | [] <- Ty.dataConFieldLabels dcon =
  let dName  = showName dflag style $ Ty.dataConName dcon
  in if last dName == '#'  -- Special case for I#, C# and so on
     then vName
     else dName ++ " " ++ newVarsSpecialSingleton vName 1 (Ty.dataConSourceArity dcon)
-- 3. Records
getDataCon dflag style vName dcon =
  let dName = showName dflag style $ Ty.dataConName dcon
      flds  = Ty.dataConFieldLabels dcon
  in dName ++ " { " ++ showFieldNames dflag style vName flds ++ " }"

-- Create a new variable by adjoining a number
newVar :: String -> Int -> String
newVar v n = v ++ show n

-- Create a list of variables which start with the same prefix
newVars :: String -> Int -> Int -> String
newVars _ _ 0 = ""
newVars v s 1 = newVar v s
newVars v s m = newVar v s ++ " " ++ newVars v (s+1) (m-1)

-- Create a list of variables which start with the same prefix
-- Special case for a single variable, in which case no number is adjoint
newVarsSpecialSingleton :: String -> Int -> Int -> String
newVarsSpecialSingleton v _     1 = v
newVarsSpecialSingleton v start n = newVars v start n

showName :: DynFlags -> PprStyle -> G.Name -> String
showName dflag style name = showOneLine dflag style $ Gap.nameForUser name

showOccName :: DynFlags -> PprStyle -> OccName -> String
showOccName dflag style name = showOneLine dflag style $ Gap.occNameForUser name

showFieldNames :: DynFlags -> PprStyle -> String -> [G.Name] -> String
showFieldNames _     _     _ []      = "" -- This should never happen
showFieldNames dflag style v (x:xs) = let fName = showName dflag style x
                                          fAcc  = fName ++ " = " ++ v ++ "_" ++ fName
                                       in case xs of
                                            [] -> fAcc
                                            _  -> fAcc ++ ", " ++ showFieldNames dflag style v xs

----------------------------------------------------------------

data SigInfo = Signature SrcSpan [G.RdrName] (G.HsType G.RdrName)
             | InstanceDecl SrcSpan G.Class

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

-- | Splitting a variable in a equation.
sig :: Options
       -> FilePath     -- ^ A target file.
       -> Int          -- ^ Line number.
       -> Int          -- ^ Column number.
       -> Ghc String
sig opt file lineNo colNo = ghandle handler body
  where
    body = inModuleContext file $ \dflag style -> do
        modSum <- Gap.fileModSummary file
        sigTy <- getSignature modSum lineNo colNo
        case sigTy of
          Nothing -> return ""
          Just (Signature loc names ty) -> do
            return $ convert opt $ ( fourInts loc
                                   , intercalate "\n"
                                       (map (initialFnBody dflag style ty) names)
                                   )
          Just (InstanceDecl loc cls) -> do
            return $ convert opt $ ( fourInts loc
                                   , intercalate "\n"
                                       (map (initialInstBody dflag style) (Ty.classMethods cls))
                                   )
            
    handler (SomeException _) = return ""

getSignature :: G.ModSummary -> Int -> Int -> Ghc (Maybe SigInfo)
getSignature modSum lineNo colNo = do
    p@ParsedModule{pm_parsed_source = ps} <- G.parseModule modSum
    -- Look into the parse tree to find the signature
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
    where obtainClassInfo minfo clsName loc = do
               tyThing <- G.modInfoLookupName minfo clsName
               case tyThing of
                 Just (Ty.ATyCon clsCon) -> 
                   case G.tyConClass_maybe clsCon of
                     Just cls -> return $ Just $ InstanceDecl loc cls
                     Nothing  -> return Nothing
                 _ -> return Nothing

-- A list of function arguments, and whether they are functions or normal arguments
-- is built from either a function signature or an instance signature
data FnArg = FnArgFunction | FnArgNormal

initialBody :: String -> [FnArg] -> String
initialBody fname args =
  case initialBodyArgs args infiniteVars infiniteFns of
    []      -> fname
    arglist -> if isSymbolName fname
               then (head arglist) ++ " " ++ fname ++ " " ++ (intercalate " " (tail arglist))
               else fname ++ " " ++ (intercalate " " arglist)
  ++ " = _" ++ fname ++ "_body"

initialBodyArgs :: [FnArg] -> [String] -> [String] -> [String]
initialBodyArgs [] _ _ = []
initialBodyArgs (FnArgFunction:xs) vs (f:fs) = f : initialBodyArgs xs vs fs
initialBodyArgs (FnArgNormal:xs)   (v:vs) fs = v : initialBodyArgs xs vs fs
initialBodyArgs _                  _      _  = error "This should never happen" -- Lists are infinite

-- Getting the initial body of function and instances differ
-- This is because for functions we only use the parsed file
-- (so the full file doesn't have to be type correct)
-- but for instances we need to get information about the class

initialFnBody :: DynFlags -> PprStyle -> G.HsType G.RdrName -> G.RdrName -> String
initialFnBody dflag style ty name = 
  let fname = showOccName dflag style $ occName name  -- get function name
      args  = \case (G.HsForAllTy _ _ _ (L _ iTy))  -> args iTy
                    (G.HsParTy (L _ iTy))           -> args iTy
                    (G.HsFunTy (L _ lTy) (L _ rTy)) -> (if fnarg lTy then FnArgFunction else FnArgNormal):args rTy
                    _ -> []
      fnarg = \case (G.HsForAllTy _ _ _ (L _ iTy)) -> fnarg iTy
                    (G.HsParTy (L _ iTy))          -> fnarg iTy
                    (G.HsFunTy _ _)                -> True
                    _                              -> False
   in initialBody fname (args ty)

initialInstBody :: DynFlags -> PprStyle -> Id -> String
initialInstBody dflag style method =
  let fname = showOccName dflag style $ G.getOccName method  -- get function name
      args  = \case ty | Just (lTy,rTy) <- Ty.splitFunTy_maybe ty ->
                      case Ty.splitFunTy_maybe lTy of
                        Just _  -> FnArgFunction:args rTy
                        Nothing -> -- Drop the class predicates
                                   if Ty.isPredTy lTy then args rTy else FnArgNormal:args rTy
                    ty | Just (_,iTy) <- Ty.splitForAllTy_maybe ty -> args iTy
                    _ -> []
   in initialBody fname (args (Ty.dropForAlls $ G.idType method))

infiniteVars, infiniteFns :: [String]
infiniteVars = infiniteSupply ["x","y","z","t","u","v","w"]
infiniteFns  = infiniteSupply ["f","g","h"]
infiniteSupply :: [String] -> [String]
infiniteSupply initialSupply = initialSupply ++ concatMap (\n -> map (\v -> v ++ show n) initialSupply) ([1 .. ] :: [Integer])

isSymbolName :: String -> Bool
isSymbolName (c:_) = c `elem` "!#$%&*+./<=>?@\\^|-~" || isSymbol c
isSymbolName []    = error "This should never happen"
