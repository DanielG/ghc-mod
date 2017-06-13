module GhcMod.Exe.Info (
    info
  , types
  ) where

import Data.Function (on)
import Data.List (sortBy)
import System.FilePath
import Exception (ghandle, SomeException(..))
import GHC (GhcMonad, SrcSpan)
import Prelude
import qualified GHC as G

import qualified GhcMod.Gap as Gap
import GhcMod.Convert
import GhcMod.Doc
import GhcMod.DynFlags
import GhcMod.Gap
import GhcMod.Logging
import GhcMod.Monad
import GhcMod.SrcUtils
import GhcMod.Types
import GhcMod.Utils (mkRevRedirMapFunc)
import GhcMod.FileMapping (fileModSummaryWithMapping)

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
      gmLog GmException "info" $ text "" $$ nest 4 (showToDoc ex)
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
      => Bool         -- ^ Include constraints into type signature
      -> FilePath     -- ^ A target file.
      -> Int          -- ^ Line number.
      -> Int          -- ^ Column number.
      -> GhcModT m String
types withConstraints file lineNo colNo =
  ghandle handler $
    runGmlT' [Left file] deferErrors $
      withInteractiveContext $ do
        crdl         <- cradle
        modSum       <- fileModSummaryWithMapping (cradleCurrentDir crdl </> file)
        srcSpanTypes <- getSrcSpanType withConstraints modSum lineNo colNo
        dflag        <- G.getSessionDynFlags
        st           <- getStyle
        convert' $ map (toTup dflag st) $ sortBy (cmp `on` fst) srcSpanTypes
 where
   handler (SomeException ex) = do
     gmLog GmException "types" $ showToDoc ex
     return []

getSrcSpanType :: (GhcMonad m) => Bool -> G.ModSummary -> Int -> Int -> m [(SrcSpan, G.Type)]
getSrcSpanType withConstraints modSum lineNo colNo =
  G.parseModule modSum
  >>= G.typecheckModule
  >>= flip (collectSpansTypes withConstraints) (lineNo, colNo)
