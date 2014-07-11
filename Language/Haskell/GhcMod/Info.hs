module Language.Haskell.GhcMod.Info (
    info
  , types
  ) where

import Control.Applicative ((<$>))
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (catMaybes)
import Exception (ghandle, SomeException(..))
import GHC (GhcMonad, LHsBind, LHsExpr, LPat, Id, TypecheckedModule(..), SrcSpan, Type)
import qualified GHC as G
import Language.Haskell.GhcMod.Doc (showPage)
import Language.Haskell.GhcMod.Gap (HasType(..))
import qualified Language.Haskell.GhcMod.Gap as Gap
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.SrcUtils
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Convert

----------------------------------------------------------------

-- | Obtaining information of a target expression. (GHCi's info:)
info :: FilePath     -- ^ A target file.
     -> Expression   -- ^ A Haskell expression.
     -> GhcMod String
info file expr = do
    opt <- options
    convert opt <$> ghandle handler body
  where
    body = inModuleContext file $ \dflag style -> do
        sdoc <- Gap.infoThing expr
        return $ showPage dflag style sdoc
    handler (SomeException _) = return "Cannot show info"

----------------------------------------------------------------

-- | Obtaining type of a target expression. (GHCi's type:)
types :: FilePath     -- ^ A target file.
      -> Int          -- ^ Line number.
      -> Int          -- ^ Column number.
      -> GhcMod String
types file lineNo colNo = do
    opt <- options
    convert opt <$> ghandle handler body
  where
    body = inModuleContext file $ \dflag style -> do
        modSum <- Gap.fileModSummary file
        srcSpanTypes <- getSrcSpanType modSum lineNo colNo
        return $ map (toTup dflag style) $ sortBy (cmp `on` fst) srcSpanTypes
    handler (SomeException _) = return []

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
