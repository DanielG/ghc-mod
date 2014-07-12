{-# LANGUAGE BangPatterns, CPP #-}

module Language.Haskell.GhcMod.Logger (
    withLogger
  , checkErrorPrefix
  ) where

import Bag (Bag, bagToList)
import Control.Applicative ((<$>),(*>))
import CoreMonad (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import ErrUtils (ErrMsg, errMsgShortDoc, errMsgExtraInfo)
import Exception (ghandle)
import GHC (DynFlags, SrcSpan, Severity(SevError))
import qualified GHC as G
import HscTypes (SourceError, srcErrorMessages)
import Language.Haskell.GhcMod.Doc (showPage, getStyle)
import Language.Haskell.GhcMod.DynFlags (withDynFlags, withCmdFlags)
import qualified Language.Haskell.GhcMod.Gap as Gap
import Language.Haskell.GhcMod.Convert (convert')
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Types (Options(..))
import Outputable (PprStyle, SDoc)
import System.FilePath (normalise)

----------------------------------------------------------------

type Builder = [String] -> [String]

newtype LogRef = LogRef (IORef Builder)

newLogRef :: IO LogRef
newLogRef = LogRef <$> newIORef id

readAndClearLogRef :: IOish m => LogRef -> GhcModT m String
readAndClearLogRef (LogRef ref) = do
    b <- liftIO $ readIORef ref
    liftIO $ writeIORef ref id
    convert' (b [])

appendLogRef :: DynFlags -> LogRef -> DynFlags -> Severity -> SrcSpan -> PprStyle -> SDoc -> IO ()
appendLogRef df (LogRef ref) _ sev src style msg = do
        let !l = ppMsg src sev df style msg
        modifyIORef ref (\b -> b . (l:))

----------------------------------------------------------------

-- | Set the session flag (e.g. "-Wall" or "-w:") then
--   executes a body. Logged messages are returned as 'String'.
--   Right is success and Left is failure.
withLogger :: IOish m
           => (DynFlags -> DynFlags)
           -> GhcModT m ()
           -> GhcModT m (Either String String)
withLogger setDF body = ghandle sourceError $ do
    logref <- liftIO $ newLogRef
    wflags <- filter ("-fno-warn" `isPrefixOf`) . ghcOpts <$> options
    withDynFlags (setLogger logref . setDF) $ do
        withCmdFlags wflags $ do body *> (Right <$> readAndClearLogRef logref)
  where
    setLogger logref df = Gap.setLogAction df $ appendLogRef df logref


----------------------------------------------------------------

-- | Converting 'SourceError' to 'String'.
sourceError :: IOish m => SourceError -> GhcModT m (Either String String)
sourceError err = do
    dflags <- G.getSessionDynFlags
    style <- toGhcMod getStyle
    ret <- convert' $ (errBagToStrList dflags style . srcErrorMessages $ err)
    return $ Left ret

errBagToStrList :: DynFlags -> PprStyle -> Bag ErrMsg -> [String]
errBagToStrList dflag style = map (ppErrMsg dflag style) . reverse . bagToList

----------------------------------------------------------------

ppErrMsg :: DynFlags -> PprStyle -> ErrMsg -> String
ppErrMsg dflag style err = ppMsg spn SevError dflag style msg ++ ext
   where
     spn = Gap.errorMsgSpan err
     msg = errMsgShortDoc err
     ext = showPage dflag style (errMsgExtraInfo err)

ppMsg :: SrcSpan -> Severity-> DynFlags -> PprStyle -> SDoc -> String
ppMsg spn sev dflag style msg = prefix ++ cts
  where
    cts  = showPage dflag style msg
    defaultPrefix
      | Gap.isDumpSplices dflag = ""
      | otherwise               = checkErrorPrefix
    prefix = fromMaybe defaultPrefix $ do
        (line,col,_,_) <- Gap.getSrcSpan spn
        file <- normalise <$> Gap.getSrcFile spn
        let severityCaption = Gap.showSeverityCaption sev
        return $ file ++ ":" ++ show line ++ ":" ++ show col ++ ":" ++ severityCaption

checkErrorPrefix :: String
checkErrorPrefix = "Dummy:0:0:Error:"
