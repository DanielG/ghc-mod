{-# LANGUAGE CPP #-}

module GhcMod.Exe.CaseSplit (
    splits
  , splits'
  , SplitResult(..)
  ) where

import Data.List (find, intercalate)
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import System.FilePath
import Prelude

import qualified DataCon as Ty
import qualified GHC as G
import Outputable (PprStyle)
import qualified TyCon as Ty
import qualified Type as Ty
import Exception

import GhcMod.Convert
import GhcMod.DynFlags
import qualified GhcMod.Gap as Gap
import GhcMod.Monad
import GhcMod.SrcUtils
import GhcMod.Doc
import GhcMod.Logging
import GhcMod.Types
import GhcMod.Utils (withMappedFile)
import GhcMod.FileMapping (fileModSummaryWithMapping)
import Control.DeepSeq

----------------------------------------------------------------
-- CASE SPLITTING
----------------------------------------------------------------

data SplitInfo = SplitInfo G.Name G.SrcSpan (G.SrcSpan, G.Type) [G.SrcSpan]
               | TySplitInfo G.Name G.SrcSpan (G.SrcSpan, Ty.Kind)
data SplitToTextInfo = SplitToTextInfo { sVarName     :: String
                                       , sBindingSpan :: G.SrcSpan
                                       , sVarSpan     :: G.SrcSpan
                                       , sTycons      :: [String]
                                       }
data SplitResult = SplitResult { sStartLine :: Int
                               , sStartCol  :: Int
                               , sEndLine   :: Int
                               , sEndCol    :: Int
                               , sNewText   :: T.Text }

-- | Splitting a variable in a equation.
-- Unlike splits', this performs parsing an type checking on every invocation.
-- This is meant for consumption by tools that call ghc-mod as a binary.
splits :: IOish m
       => FilePath     -- ^ A target file.
       -> Int          -- ^ Line number.
       -> Int          -- ^ Column number.
       -> GhcModT m String
splits file lineNo colNo =
  ghandle handler $ runGmlT' [Left file] deferErrors $ do
      oopts <- outputOpts
      crdl <- cradle
      modSum <- fileModSummaryWithMapping (cradleCurrentDir crdl </> file)
      p <- G.parseModule modSum
      tcm <- G.typecheckModule p
      whenFound' oopts (performSplit file tcm lineNo colNo) $
        \(SplitResult sLine sCol eLine eCol newText) ->
          return $!! ((sLine, sCol, eLine, eCol), T.unpack newText)
 where
   handler (SomeException ex) = do
     gmLog GmException "splits" $
           text "" $$ nest 4 (showToDoc ex)
     emptyResult =<< outputOpts

-- | Split an identifier in a function definition.
-- Meant for library-usage.
splits' :: IOish m => FilePath -> G.TypecheckedModule -> Int -> Int -> GhcModT m (Maybe SplitResult)
splits' file tcm lineNo colNo =
  ghandle handler $ runGmlT' [Left file] deferErrors $ performSplit file tcm lineNo colNo
  where
    handler (SomeException ex) = do
      gmLog GmException "splits'" $
            text "" $$ nest 4 (showToDoc ex)
      return Nothing

performSplit :: IOish m => FilePath -> G.TypecheckedModule -> Int -> Int -> GmlT m (Maybe SplitResult)
performSplit file tcm lineNo colNo = do
  style <- getStyle
  dflag <- G.getSessionDynFlags
  maybeSplitInfo <- getSrcSpanTypeForSplit tcm lineNo colNo
  sequenceA $ constructSplitResult file maybeSplitInfo dflag style

constructSplitResult :: (IOish m) => FilePath -> Maybe SplitInfo -> G.DynFlags -> PprStyle -> Maybe (GmlT m SplitResult)
constructSplitResult file maybeSplitInfo dflag style = do
  splitInfo <- maybeSplitInfo
  let splitToTextInfo = constructSplitToTextInfo splitInfo dflag style
  startLoc <- maybeSrcSpanStart $ sBindingSpan splitToTextInfo
  endLoc <- maybeSrcSpanEnd $ sBindingSpan splitToTextInfo
  let startLine = G.srcLocLine startLoc
      startCol  = G.srcLocCol startLoc
      endLine   = G.srcLocLine endLoc
      endCol    = G.srcLocCol endLoc
      newText   = genCaseSplitTextFile file splitToTextInfo
  return $ SplitResult startLine startCol endLine endCol . T.pack <$> newText

constructSplitToTextInfo :: SplitInfo -> G.DynFlags -> PprStyle -> SplitToTextInfo
constructSplitToTextInfo splitInfo dflag style =
  SplitToTextInfo varName' bndLoc varLoc typeCons
  where
    typeCons = getTyCons dflag style varName varT
    varName' = showName dflag style varName  -- Convert name to string
    (varName, bndLoc, varLoc, varT) = case splitInfo of
      (SplitInfo vn bl (vl, vt) _matches) -> (vn, bl, vl, vt)
      (TySplitInfo vn bl (vl, vt)) -> (vn, bl, vl, vt)

maybeSrcSpanStart :: G.SrcSpan -> Maybe G.RealSrcLoc
maybeSrcSpanStart s = case G.srcSpanStart s of
  (G.RealSrcLoc startLoc) -> Just startLoc
  _ -> Nothing

maybeSrcSpanEnd :: G.SrcSpan -> Maybe G.RealSrcLoc
maybeSrcSpanEnd s = case G.srcSpanEnd s of
  (G.RealSrcLoc endLoc) -> Just endLoc
  _ -> Nothing
----------------------------------------------------------------
-- a. Code for getting the information of the variable

getSrcSpanTypeForSplit :: G.GhcMonad m => G.TypecheckedModule -> Int -> Int -> m (Maybe SplitInfo)
getSrcSpanTypeForSplit tcm lineNo colNo = do
  fn <- getSrcSpanTypeForFnSplit tcm lineNo colNo
  if isJust fn
     then return fn
     else getSrcSpanTypeForTypeSplit tcm lineNo colNo

-- Information for a function case split
getSrcSpanTypeForFnSplit :: G.GhcMonad m => G.TypecheckedModule -> Int -> Int -> m (Maybe SplitInfo)
getSrcSpanTypeForFnSplit tcm@G.TypecheckedModule{G.tm_typechecked_source = tcs} lineNo colNo = do
    let varPat = find isPatternVar $ listifySpans tcs (lineNo, colNo) :: Maybe (G.LPat Gap.GhcTc)
        match  = last $ listifySpans tcs (lineNo, colNo) :: G.LMatch Gap.GhcTc (G.LHsExpr Gap.GhcTc)
    case varPat of
      Nothing  -> return Nothing
      Just varPat' -> do
        varT <- Gap.getType tcm varPat'  -- Finally we get the type of the var
        case varT of
          Just varT' ->
            let (G.L matchL (G.Match { G.m_grhss = G.GRHSs { G.grhssGRHSs = rhsLs }})) = match
            in return $ Just (SplitInfo (getPatternVarName varPat') matchL varT' (map G.getLoc rhsLs) )
          _ -> return Nothing

isPatternVar :: G.LPat Gap.GhcTc -> Bool
isPatternVar (G.L _ (G.VarPat {})) = True
isPatternVar _                     = False

getPatternVarName :: G.LPat Gap.GhcTc -> G.Name
#if __GLASGOW_HASKELL__ >= 806
getPatternVarName (G.L _ (G.VarPat _ (G.L _ vName))) = G.getName vName
#elif __GLASGOW_HASKELL__ >= 800
getPatternVarName (G.L _ (G.VarPat (G.L _ vName))) = G.getName vName
#else
getPatternVarName (G.L _ (G.VarPat vName)) = G.getName vName
#endif
getPatternVarName _                      = error "This should never happened"

-- TODO: Information for a type family case split
getSrcSpanTypeForTypeSplit :: G.GhcMonad m => G.TypecheckedModule -> Int -> Int -> m (Maybe SplitInfo)
getSrcSpanTypeForTypeSplit _tcm _lineNo _colNo = return Nothing

----------------------------------------------------------------
-- b. Code for getting the possible constructors

getTyCons :: G.DynFlags -> PprStyle -> G.Name -> G.Type ->  [String]
getTyCons dflag style name ty | Just (tyCon, _) <- Ty.splitTyConApp_maybe ty =
  let name' = showName dflag style name  -- Convert name to string
  in getTyCon dflag style name' tyCon
getTyCons dflag style name _ = [showName dflag style name]

-- Write cases for one type
getTyCon :: G.DynFlags -> PprStyle -> String -> Ty.TyCon -> [String]
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
getDataCon :: G.DynFlags -> PprStyle -> String -> Ty.DataCon -> String
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
#if __GLASGOW_HASKELL__ >= 800
      flds  = map Ty.flSelector $ Ty.dataConFieldLabels dcon
#else
      flds  = Ty.dataConFieldLabels dcon
#endif
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

showFieldNames :: G.DynFlags -> PprStyle -> String -> [G.Name] -> String
showFieldNames _     _     _ []      = "" -- This should never happen
showFieldNames dflag style v (x:xs) = let fName = showName dflag style x
                                          fAcc  = fName ++ " = " ++ v ++ "_" ++ fName
                                       in case xs of
                                            [] -> fAcc
                                            _  -> fAcc ++ ", " ++ showFieldNames dflag style v xs

----------------------------------------------------------------
-- c. Code for performing the case splitting


genCaseSplitTextFile :: IOish m =>
    FilePath -> SplitToTextInfo -> GmlT m String
genCaseSplitTextFile file info =
  withMappedFile file $ \file' -> liftIO $ do
    t <- T.readFile file'
    return $ getCaseSplitText (T.lines t) info

getCaseSplitText :: [T.Text] -> SplitToTextInfo -> String
getCaseSplitText t SplitToTextInfo{ sVarName = sVN, sBindingSpan = sBS
                                       , sVarSpan = sVS, sTycons = sT }  =
  let bindingText = getBindingText t sBS
      difference  = srcSpanDifference sBS sVS
      replaced    = map (replaceVarWithTyCon bindingText difference sVN) sT
      -- The newly generated bindings need to be indented to align with the
      -- original binding.
      replaced'   = head replaced : map (indentBindingTo sBS) (tail replaced)
   in T.unpack $ T.intercalate (T.pack "\n") (concat replaced')

getBindingText :: [T.Text] -> G.SrcSpan -> [T.Text]
getBindingText t srcSpan =
  let Just (sl,sc,el,ec) = Gap.getSrcSpan srcSpan
      lines_ = drop (sl - 1) $ take el t
   in if sl == el
      then -- only one line
           [T.drop (sc - 1) $ T.take ec $ head lines_]
      else -- several lines
            let (first,rest,last_) = (head lines_, tail $ init lines_, last lines_)
             in T.drop (sc - 1) first : rest ++ [T.take ec last_]

srcSpanDifference :: G.SrcSpan -> G.SrcSpan -> (Int,Int,Int,Int)
srcSpanDifference b v =
  let Just (bsl,bsc,_   ,_)  = Gap.getSrcSpan b
      Just (vsl,vsc,vel,vec) = Gap.getSrcSpan v
   in (vsl - bsl, vsc - bsc, vel - bsl, vec - bsc) -- assume variable in one line

replaceVarWithTyCon :: [T.Text] -> (Int,Int,Int,Int) -> String -> String -> [T.Text]
replaceVarWithTyCon t (vsl,vsc,_,vec) varname tycon =
  let tycon'      = if ' ' `elem` tycon || ':' `elem` tycon then "(" ++ tycon ++ ")" else tycon
      lengthDiff  = length tycon' - length varname
      tycon''     = T.pack $ if lengthDiff < 0 then tycon' ++ replicate (-lengthDiff) ' ' else tycon'
      spacesToAdd = if lengthDiff < 0 then 0 else lengthDiff
   in zipWith (\n line -> if n < vsl
                          then line  -- before variable starts
                          else if n == vsl
                               then T.take vsc line `T.append` tycon'' `T.append` T.drop vec line
                               else T.replicate spacesToAdd (T.pack " ") `T.append` line)
              [0 ..] t

indentBindingTo :: G.SrcSpan -> [T.Text] -> [T.Text]
indentBindingTo bndLoc binds =
  let Just (_,sl,_,_) = Gap.getSrcSpan bndLoc
      indent      = (T.replicate (sl - 1) (T.pack " ") `T.append`)
   in indent (head binds) : tail binds
