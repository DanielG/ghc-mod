module Language.Haskell.GhcMod.Info (
    info
  , types
  ) where

import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (catMaybes, mapMaybe)
import System.FilePath
import Exception (ghandle, SomeException(..))
import GHC (GhcMonad, LHsBind, LHsExpr, LPat, Id, TypecheckedModule(..), SrcSpan)
import Prelude
import qualified GHC as G
import qualified Var as G (varType)
import qualified Language.Haskell.GhcMod.Gap as Gap

import Language.Haskell.GhcMod.Convert
import Language.Haskell.GhcMod.Doc
import Language.Haskell.GhcMod.DynFlags
import Language.Haskell.GhcMod.Gap
import Language.Haskell.GhcMod.Logging
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.SrcUtils
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Utils (mkRevRedirMapFunc)
import Language.Haskell.GhcMod.FileMapping (fileModSummaryWithMapping)
import Control.Applicative
import Type
import Control.Arrow

----------------------------------------------------------------

-- | Obtaining information of a target expression. (GHCi's info:)
info :: IOish m
     => FilePath     -- ^ A target file.
     -> Expression   -- ^ A Haskell expression.
     -> GhcModT m String
info file expr =
  ghandle handler $
    runGmlT' [Left file] deferErrors $
      withInteractiveContext $ do
        convert' =<< body
  where
    handler (SomeException ex) = do
      gmLog GmException "info" $ text "" $$ nest 4 (showDoc ex)
      convert' "Cannot show info"

    body :: (GhcMonad m, GmState m, GmEnv m) => m String
    body = do
      m <- mkRevRedirMapFunc
      sdoc  <- Gap.infoThing m expr
      st    <- getStyle
      dflag <- G.getSessionDynFlags
      return $ showPage dflag st sdoc

----------------------------------------------------------------

-- | Obtaining type of a target expression. (GHCi's type:)
types :: IOish m
      => FilePath     -- ^ A target file.
      -> Int          -- ^ Line number.
      -> Int          -- ^ Column number.
      -> GhcModT m String
types file lineNo colNo =
  ghandle handler $
    runGmlT' [Left file] deferErrors $
      withInteractiveContext $ do
        crdl         <- cradle
        modSum       <- fileModSummaryWithMapping (cradleCurrentDir crdl </> file)
        srcSpanTypes <- getSrcSpanType modSum lineNo colNo
        dflag        <- G.getSessionDynFlags
        st           <- getStyle
        convert' $ map (toTup dflag st) $ sortBy (cmp `on` fst) srcSpanTypes
 where
   handler (SomeException ex) = do
     gmLog GmException "types" $ showDoc ex
     return []

getSrcSpanType :: GhcMonad m => G.ModSummary -> Int -> Int -> m [(SrcSpan, Type)]
getSrcSpanType modSum lineNo colNo = do
  p <- G.parseModule modSum
  tcm@TypecheckedModule{tm_typechecked_source = tcs} <- G.typecheckModule p
  let bs = listifySpans tcs (lineNo, colNo) :: [LHsBind Id]
      es = listifySpans tcs (lineNo, colNo) :: [LHsExpr Id]
      ps = listifySpans tcs (lineNo, colNo) :: [LPat Id]
      as = concatMap abtoex $ listifyAbsBinds tcs
      abtoex (G.L _spn G.AbsBinds{abs_exports = es'})
        = liftA2 (,) G.abe_mono (G.varType . G.abe_poly) `map` es'
      abtoex _ = []
      getType' b = getType tcm b >>= tryGetConstrainedType b
      tryGetConstrainedType _ Nothing = return Nothing
      tryGetConstrainedType b (Just gt) =
        do
          ids <- getId tcm b
          return $ ct ids <|> Just gt
        where
          ct [pid] = (,) (fst gt) <$> lookup pid as
          ct []    = Nothing
          -- TODO: A case of multiple ids should probably
          -- collect all constraints and then apply
          -- them to calculated type. No idea how
          -- to do that at the moment.
          -- NB: The following does not work, since
          -- "inner" types have different IDs from
          -- exported types. So we need some sort of
          -- type substitution.
          ct pids  =
            let
              ctys  = mapMaybe (`lookup` as) pids
              preds = concatMap (fst . getPreds) ctys
              --typs  = map (snd . getPreds) ctys
              ty    = mkFunTys preds $ snd gt
            in Just (fst gt, ty)
          getPreds x | isForAllTy x = getPreds $ dropForAlls x
                     | Just (c, t) <- splitFunTy_maybe x
                     , isPredTy c = first (c:) $ getPreds t
                     | otherwise = ([], x)

  ets <- mapM getType' es
  bts <- mapM getType' bs
  pts <- mapM getType' ps
  return $ catMaybes $ concat [ets, bts, pts]
