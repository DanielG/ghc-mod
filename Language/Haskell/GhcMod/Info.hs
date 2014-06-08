{-# LANGUAGE TupleSections, FlexibleInstances, Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Haskell.GhcMod.Info (
    infoExpr
  , info
  , typeExpr
  , types
  , splitVar
  , splits
  ) where

import Control.Applicative ((<$>))
import CoreMonad (liftIO)
import CoreUtils (exprType)
import Data.Function (on)
import Data.Generics
import Data.List (find, sortBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord as O
import Exception (ghandle, SomeException(..))
import GHC (Ghc, LHsBind, LHsExpr, LPat, Id, TypecheckedModule(..), DynFlags, SrcSpan, Type, Located, TypecheckedSource, GenLocated(L))
import qualified GHC as G
import GHC.SYB.Utils (Stage(TypeChecker), everythingStaged)
import Language.Haskell.GhcMod.Doc (showPage, showOneLine, getStyle)
import Language.Haskell.GhcMod.GHCApi
import Language.Haskell.GhcMod.Gap (HasType(..), setDeferTypeErrors)
import qualified Language.Haskell.GhcMod.Gap as Gap
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Convert
import Outputable (PprStyle)
import TcHsSyn (hsPatType)
import qualified Type as Ty
import qualified TyCon as Ty
import qualified DataCon as Ty

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
    withDynFlags (setDeferTypeErrors . setNoWaringFlags) $ do
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
  map (getTyCon dflag style name) (Ty.tyConDataCons tyCon)
getTyCons dflag style name _ = [showName dflag style name]

getTyCon :: DynFlags -> PprStyle -> G.Name -> Ty.DataCon -> String
getTyCon dflag style vName dcon =
  let dName  = showName dflag style $ Ty.dataConName dcon
      vName' = showName dflag style vName
  in if Ty.dataConIsInfix dcon
     then -- We have an infix constructor
          case Ty.dataConSourceArity dcon of
            0 -> dName
            1 -> vName' ++ dName
            n -> newVar vName' 1 ++ " " ++ dName ++ " " ++ newVars vName' 2 (n-1)
     else case Ty.dataConFieldLabels dcon of
            []   -> -- We have a non-record constructor
                    dName ++ " " ++ newVarsSpecialSingleton vName' 1 (Ty.dataConSourceArity dcon)
            flds -> -- We have a record constructor
                    dName ++ " { " ++ showFieldNames dflag style vName' flds ++ " }"
            
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

showFieldNames :: DynFlags -> PprStyle -> String -> [G.Name] -> String
showFieldNames _     _     _ []      = "" -- This should never happen
showFieldNames dflag style v (x:xs) = let fName = showName dflag style x
                                          fAcc  = fName ++ " = " ++ v ++ "_" ++ fName
                                       in case xs of
                                            [] -> fAcc
                                            _  -> fAcc ++ ", " ++ showFieldNames dflag style v xs
