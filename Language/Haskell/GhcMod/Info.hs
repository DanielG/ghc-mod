{-# LANGUAGE TupleSections #-}

module Language.Haskell.GhcMod.Info (
    info
  , types
  ) where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (catMaybes)
import System.FilePath
import Exception (ghandle, SomeException(..))
import GHC (GhcMonad, LHsBind, LHsExpr, LPat, Id, TypecheckedModule(..), SrcSpan, Type)
import RdrName (GlobalRdrElt(gre_name), lookupGRE_RdrName)
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
      withInteractiveContext $
        convert <$> options <*> body
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

getRdrType :: (GhcMonad m) => G.RdrName -> m (Maybe Type)
getRdrType rdr_name = runMaybeT $ do
  rdr_env <- lift G.getGRE
  gre_elt:_ <- return $ lookupGRE_RdrName rdr_name rdr_env
  G.AnId i <- MaybeT $ G.lookupName (gre_name gre_elt)
  return $ G.idType i

getSrcSpanType :: (GhcMonad m, GmLog m, GmEnv m, MonadIO m)
               => G.ModSummary -> Int -> Int -> m [(SrcSpan, Type)]
getSrcSpanType modSum lineNo colNo = do
  p@G.ParsedModule{pm_parsed_source = psrc} <- G.parseModule modSum
  -- In the parsed AST we may find the span in,
  --   * A type signature (e.g. the name on the LHS of a binding)
  -- These are RdrNames
  let is = listifyParsedSpans psrc (lineNo, colNo) :: [G.Located G.RdrName]

  tcm@TypecheckedModule{tm_typechecked_source = tcs} <- G.typecheckModule p
  -- In the typechecked AST we may find the span in,
  --   * A binding
  --   * An expression
  --   * A pattern
  let bs = listifySpans tcs (lineNo, colNo) :: [LHsBind Id]
      es = listifySpans tcs (lineNo, colNo) :: [LHsExpr Id]
      ps = listifySpans tcs (lineNo, colNo) :: [LPat Id]
  its <- mapM (\(G.L l i) -> fmap (l,) <$> getRdrType i) is
  bts <- mapM (getType tcm) bs
  ets <- mapM (getType tcm) es
  pts <- mapM (getType tcm) ps
  return $ catMaybes $ concat [ets, its, bts, pts]
