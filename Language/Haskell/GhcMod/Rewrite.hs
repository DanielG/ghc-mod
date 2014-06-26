{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Language.Haskell.GhcMod.Rewrite (
    splitVar
  , splits
  , fillSig
  , sig
  ) where

import Data.Char (isSymbol)
import Data.List (find, intercalate)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import Exception (ghandle, SomeException(..))
import GHC (Ghc, LHsBind, LHsExpr, LPat, Id, ParsedModule(..), TypecheckedModule(..), DynFlags, SrcSpan, Type, GenLocated(L))
import qualified GHC as G
import Language.Haskell.GhcMod.Doc (showOneLine)
import Language.Haskell.GhcMod.GHCApi
import Language.Haskell.GhcMod.Gap (HasType(..))
import qualified Language.Haskell.GhcMod.Gap as Gap
import Language.Haskell.GhcMod.SrcUtils
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Convert
import MonadUtils (liftIO)
import Outputable (PprStyle)
import qualified Type as Ty
import qualified TyCon as Ty
import qualified DataCon as Ty
import qualified HsBinds as Ty
import qualified Class as Ty
import OccName (OccName, occName)
import qualified Language.Haskell.Exts.Annotated as HE

----------------------------------------------------------------

data SplitInfo = SplitInfo G.Name (SrcSpan,Type) (SrcSpan, Type) [SrcSpan]
data SplitToTextInfo = SplitToTextInfo { sVarName     :: String
                                       , sBindingSpan :: SrcSpan
                                       , sVarSpan     :: SrcSpan
                                       , sMatchesSpan :: [SrcSpan]
                                       , sTycons      :: [String]
                                       }

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
          Nothing -> return $ convert opt ([] :: [String])
          Just (SplitInfo varName (bndLoc,_) (varLoc,varT) matches) -> do
            let varName' = showName dflag style varName  -- Convert name to string
            text <- genCaseSplitTextFile file (SplitToTextInfo varName' bndLoc varLoc matches $
                                               getTyCons dflag style varName varT)
            return $ convert opt $ ( fourInts bndLoc
                                   , text)
    handler (SomeException _) = return $ convert opt ([] :: [String])

getSrcSpanTypeForSplit :: G.ModSummary -> Int -> Int -> Ghc (Maybe SplitInfo)
getSrcSpanTypeForSplit modSum lineNo colNo = do
    p@ParsedModule{pm_parsed_source = pms} <- G.parseModule modSum
    tcm@TypecheckedModule{tm_typechecked_source = tcs} <- G.typecheckModule p
    let bs:_ = listifySpans tcs (lineNo, colNo) :: [LHsBind Id]
        varPat  = find isPatternVar $ listifySpans tcs (lineNo, colNo) :: Maybe (LPat Id)
        match:_ = listifyParsedSpans pms (lineNo, colNo) :: [G.LMatch G.RdrName (LHsExpr G.RdrName)]
    case varPat of
      Nothing  -> return Nothing
      Just varPat' -> do
        varT <- getType tcm varPat'  -- Finally we get the type of the var
        bsT  <- getType tcm bs
        case (varT, bsT) of
          (Just varT', Just (_,bsT')) ->
            let (L matchL (G.Match _ _ (G.GRHSs rhsLs _))) = match
            in return $ Just (SplitInfo (getPatternVarName varPat') (matchL,bsT') varT' (map G.getLoc rhsLs) )
          _ -> return Nothing

isPatternVar :: LPat Id -> Bool
isPatternVar (L _ (G.VarPat _)) = True
isPatternVar _                  = False

getPatternVarName :: LPat Id -> G.Name
getPatternVarName (L _ (G.VarPat vName)) = G.getName vName
getPatternVarName _                      = error "This should never happend"

----------------------------------------------------------------

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
     else case Ty.dataConSourceArity dcon of
            0 -> dName
            _ -> dName ++ " " ++ newVarsSpecialSingleton vName 1 (Ty.dataConSourceArity dcon)
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

genCaseSplitTextFile :: FilePath -> SplitToTextInfo -> Ghc String
genCaseSplitTextFile file info = liftIO $ do
  text <- T.readFile file
  return $ getCaseSplitText (T.lines text) info

getCaseSplitText :: [T.Text] -> SplitToTextInfo -> String
getCaseSplitText text (SplitToTextInfo { .. })  =
  let bindingText = getBindingText text sBindingSpan
      difference  = srcSpanDifference sBindingSpan sVarSpan
      replaced    = concatMap (replaceVarWithTyCon bindingText difference sVarName) sTycons
   in T.unpack $ T.intercalate (T.pack "\n") replaced

getBindingText :: [T.Text] -> SrcSpan -> [T.Text]
getBindingText text srcSpan =
  let Just (sl,sc,el,ec) = Gap.getSrcSpan srcSpan
      lines_ = drop (sl - 1) $ take el text
   in if sl == el
      then -- only one line
           [T.drop (sc - 1) $ T.take ec $ head lines_]
      else -- several lines
            let (first,rest,last_) = (head lines_, tail $ init lines_, last lines_)
             in (T.drop (sc - 1) first) : rest ++ [T.take ec last_]

srcSpanDifference :: SrcSpan -> SrcSpan -> (Int,Int,Int,Int)
srcSpanDifference b v =
  let Just (bsl,bsc,_   ,_)  = Gap.getSrcSpan b
      Just (vsl,vsc,vel,vec) = Gap.getSrcSpan v
   in (vsl - bsl, vsc - bsc, vel - bsl, vec - bsc) -- assume variable in one line

replaceVarWithTyCon :: [T.Text] -> (Int,Int,Int,Int) -> String -> String -> [T.Text]
replaceVarWithTyCon text (vsl,vsc,_,vec) varname tycon = 
  let tycon'      = if ' ' `elem` tycon || ':' `elem` tycon then "(" ++ tycon ++ ")" else tycon
      lengthDiff  = length tycon' - length varname
      tycon''     = T.pack $ if lengthDiff < 0 then tycon' ++ replicate (-lengthDiff) ' ' else tycon'
      spacesToAdd = if lengthDiff < 0 then 0 else lengthDiff
   in zipWith (\n line -> if n < vsl
                          then line  -- before variable starts
                          else if n == vsl
                               then T.take vsc line `T.append` tycon'' `T.append` T.drop vec line
                               else T.replicate spacesToAdd (T.pack " ") `T.append` line)
              [0 ..] text
----------------------------------------------------------------

data SigInfo = Signature SrcSpan [G.RdrName] (G.HsType G.RdrName)
             | InstanceDecl SrcSpan G.Class

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
          Nothing -> return $ convert opt ([] :: [String])
          Just (Signature loc names ty) -> do
            return $ convert opt $ ( "function"
                                   , fourInts loc
                                   , map (initialFnBody dflag style ty) names
                                   )
         
          Just (InstanceDecl loc cls) -> do
            return $ convert opt $ ( "instance"
                                   , fourInts loc
                                   , map (initialInstBody dflag style) (Ty.classMethods cls)
                                   )
            
    handler (SomeException _) = do
      -- Fallback: try to get information via haskell-src-exts
      sigTy <- getSignatureFromHE file lineNo colNo
      case sigTy of
        Just (HESignature loc names ty) -> do
          return $ convert opt $ ( "function"
                                 , (HE.srcSpanStartLine loc
                                   ,HE.srcSpanStartColumn loc
                                   ,HE.srcSpanEndLine loc
                                   ,HE.srcSpanEndColumn loc)
                                 , map (initialHEFnBody  ty) names
                                 )
        _ -> return $ convert opt ([] :: [String])

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

obtainClassInfo :: G.ModuleInfo -> G.Name -> SrcSpan -> Ghc (Maybe SigInfo)
obtainClassInfo minfo clsName loc = do
  tyThing <- G.modInfoLookupName minfo clsName
  case tyThing of
    Just (Ty.ATyCon clsCon) -> 
      case G.tyConClass_maybe clsCon of
        Just cls -> return $ Just $ InstanceDecl loc cls
        Nothing  -> return Nothing
    _ -> return Nothing

getSignatureFromHE :: FilePath -> Int -> Int -> Ghc (Maybe HESigInfo)
getSignatureFromHE file lineNo colNo = do
  presult <- liftIO $ HE.parseFile file
  case presult of
    HE.ParseOk (HE.Module _ _ _ _ mdecls) -> do
      let tsig = find (typeSigInRange lineNo colNo) mdecls
      case tsig of
        Just (HE.TypeSig (HE.SrcSpanInfo s _) names ty) ->
          return $ Just (HESignature s names ty)
        _ -> return Nothing
    _ -> return Nothing

typeSigInRange :: Int -> Int -> HE.Decl HE.SrcSpanInfo -> Bool
typeSigInRange lineNo colNo (HE.TypeSig (HE.SrcSpanInfo s _)  _ _) =
  HE.srcSpanStart s <= (lineNo, colNo) && HE.srcSpanEnd s >= (lineNo, colNo)
typeSigInRange _  _ _= False

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

initialHEFnBody :: HE.Type HE.SrcSpanInfo -> HE.Name HE.SrcSpanInfo -> String
initialHEFnBody ty name = 
  let fname = case name of
                HE.Ident  _ s -> s
                HE.Symbol _ s -> s
      args  = \case (HE.TyForall _ _ _ iTy) -> args iTy
                    (HE.TyParen _ iTy)      -> args iTy
                    (HE.TyFun _ lTy rTy)    -> (if fnarg lTy then FnArgFunction else FnArgNormal):args rTy
                    _ -> []
      fnarg = \case (HE.TyForall _ _ _ iTy) -> fnarg iTy
                    (HE.TyParen _ iTy)      -> fnarg iTy
                    (HE.TyFun _ _ _)        -> True
                    _                       -> False
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
