{-# LANGUAGE CPP #-}

module Language.Haskell.GhcMod.CaseSplit (
  splits
  ) where

import Data.List (find, intercalate)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import Exception (ghandle, SomeException(..))
import GHC (GhcMonad, LHsBind, LHsExpr, LPat, Id, ParsedModule(..), TypecheckedModule(..), DynFlags, SrcSpan, Type, GenLocated(L))
import qualified GHC as G
import Language.Haskell.GhcMod.Gap (HasType(..))
import qualified Language.Haskell.GhcMod.Gap as Gap
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.SrcUtils
import Language.Haskell.GhcMod.Convert
import CoreMonad (liftIO)
import Outputable (PprStyle)
import qualified Type as Ty
import qualified TyCon as Ty
import qualified DataCon as Ty

----------------------------------------------------------------
-- CASE SPLITTING
----------------------------------------------------------------

data SplitInfo = SplitInfo G.Name (SrcSpan,Type) (SrcSpan, Type) [SrcSpan]
data SplitToTextInfo = SplitToTextInfo { sVarName     :: String
                                       , sBindingSpan :: SrcSpan
                                       , sVarSpan     :: SrcSpan
                                       , sTycons      :: [String]
                                       }

-- | Splitting a variable in a equation.
splits :: FilePath     -- ^ A target file.
       -> Int          -- ^ Line number.
       -> Int          -- ^ Column number.
       -> GhcMod String
splits file lineNo colNo = ghandle handler body
  where
    body = inModuleContext file $ \dflag style -> do
        opt <- options
        modSum <- Gap.fileModSummary file
        whenFound' opt (getSrcSpanTypeForSplit modSum lineNo colNo) $
          \(SplitInfo varName (bndLoc,_) (varLoc,varT) _matches) -> do
             let varName' = showName dflag style varName  -- Convert name to string
             text <- genCaseSplitTextFile file (SplitToTextInfo varName' bndLoc varLoc $
                                                getTyCons dflag style varName varT)
             return (fourInts bndLoc, text)
    handler (SomeException _) = emptyResult =<< options

----------------------------------------------------------------
-- a. Code for getting the information of the variable

getSrcSpanTypeForSplit :: GhcMonad m => G.ModSummary -> Int -> Int -> m (Maybe SplitInfo)
getSrcSpanTypeForSplit modSum lineNo colNo = do
    p@ParsedModule{pm_parsed_source = pms} <- G.parseModule modSum
    tcm@TypecheckedModule{tm_typechecked_source = tcs} <- G.typecheckModule p
    let bs:_ = listifySpans tcs (lineNo, colNo) :: [LHsBind Id]
        varPat  = find isPatternVar $ listifySpans tcs (lineNo, colNo) :: Maybe (LPat Id)
        match:_ = listifyParsedSpans pms (lineNo, colNo)
#if __GLASGOW_HASKELL__ < 708
                    :: [G.LMatch G.RdrName]
#else
                    :: [G.LMatch G.RdrName (LHsExpr G.RdrName)]
#endif
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
-- b. Code for getting the possible constructors

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

showFieldNames :: DynFlags -> PprStyle -> String -> [G.Name] -> String
showFieldNames _     _     _ []      = "" -- This should never happen
showFieldNames dflag style v (x:xs) = let fName = showName dflag style x
                                          fAcc  = fName ++ " = " ++ v ++ "_" ++ fName
                                       in case xs of
                                            [] -> fAcc
                                            _  -> fAcc ++ ", " ++ showFieldNames dflag style v xs

----------------------------------------------------------------
-- c. Code for performing the case splitting

genCaseSplitTextFile :: GhcMonad m => FilePath -> SplitToTextInfo -> m String
genCaseSplitTextFile file info = liftIO $ do
  text <- T.readFile file
  return $ getCaseSplitText (T.lines text) info

getCaseSplitText :: [T.Text] -> SplitToTextInfo -> String
getCaseSplitText text (SplitToTextInfo { sVarName = sVN, sBindingSpan = sBS
                                       , sVarSpan = sVS, sTycons = sT })  =
  let bindingText = getBindingText text sBS
      difference  = srcSpanDifference sBS sVS
      replaced    = concatMap (replaceVarWithTyCon bindingText difference sVN) sT
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
