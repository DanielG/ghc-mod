module Language.Haskell.GhcMod.Info (
    info
  , types
  ) where

import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (catMaybes)
import System.FilePath
import Exception (ghandle, SomeException(..))
import GHC (GhcMonad, LHsBind, LHsExpr, LPat, Id, TypecheckedModule(..), SrcSpan, Type)
import Prelude
import qualified GHC as G
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
  bts <- mapM (getType tcm) bs
  ets <- mapM (getType tcm) es
  pts <- mapM (getType tcm) ps
  return $ catMaybes $ concat [ets, bts, pts]
