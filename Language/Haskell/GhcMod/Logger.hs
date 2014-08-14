{-# LANGUAGE CPP #-}

module Language.Haskell.GhcMod.Logger (
    withLogger
  , withLoggerTwice
  , checkErrorPrefix
  ) where

import Bag (Bag, bagToList, filterBag, unionBags)
import Control.Applicative ((<$>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.List (isPrefixOf, find)
import Data.Maybe (fromMaybe, isJust)
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
import Language.Haskell.GhcMod.Types
import Outputable (PprStyle, SDoc)
import System.FilePath (normalise)

----------------------------------------------------------------

type Builder = [String] -> [String]

data Log = Log [String] Builder

newtype LogRef = LogRef (IORef Log)

emptyLog :: Log
emptyLog = Log [] id

newLogRef :: IO LogRef
newLogRef = LogRef <$> newIORef emptyLog

readAndClearLogRef :: IOish m => LogRef -> GhcModT m String
readAndClearLogRef (LogRef ref) = do
    Log _ b <- liftIO $ readIORef ref
    liftIO $ writeIORef ref emptyLog
    convert' (b [])

appendLogRef :: DynFlags -> LogRef -> DynFlags -> Severity -> SrcSpan -> PprStyle -> SDoc -> IO ()
appendLogRef df (LogRef ref) _ sev src style msg = modifyIORef ref update
  where
    l = ppMsg src sev df style msg
    update lg@(Log ls b)
      | l `elem` ls = lg
      | otherwise   = Log (l:ls) (b . (l:))

----------------------------------------------------------------

-- | Set the session flag (e.g. "-Wall" or "-w:") then
--   executes a body. Logged messages are returned as 'String'.
--   Right is success and Left is failure.
withLogger :: IOish m
           => (DynFlags -> DynFlags)
           -> GhcModT m ()
           -> GhcModT m (Either String String)
withLogger setDF body = ghandle sourceError $ do
    logref <- liftIO newLogRef
    wflags <- filter ("-fno-warn" `isPrefixOf`) . ghcUserOptions <$> options
    withDynFlags (setLogger logref . setDF) $
        withCmdFlags wflags $ do
            body
            Right <$> readAndClearLogRef logref
  where
    setLogger logref df = Gap.setLogAction df $ appendLogRef df logref

withLoggerTwice :: IOish m
                => (DynFlags -> DynFlags)
                -> GhcModT m ()
                -> (DynFlags -> DynFlags)
                -> GhcModT m ()
                -> GhcModT m (Either String String)
withLoggerTwice setDF1 body1 setDF2 body2 = do
  err1 <- ghandle sourceErrorBag $ do
    logref <- liftIO newLogRef
    wflags <- filter ("-fno-warn" `isPrefixOf`) . ghcUserOptions <$> options
    withDynFlags (setLogger logref . setDF1) $
        withCmdFlags wflags $ do
            body1
            Right <$> readAndClearLogRef logref
  err2 <- ghandle sourceErrorBag $ do
    logref <- liftIO newLogRef
    wflags <- filter ("-fno-warn" `isPrefixOf`) . ghcUserOptions <$> options
    withDynFlags (setLogger logref . setDF2) $
        withCmdFlags wflags $ do
            body2
            Right <$> readAndClearLogRef logref
  case (err1, err2) of
    (Right x, Right _) -> return $ Right x
    (Left b1, Right _) -> errBagToStr b1
    (Right _, Left b2) -> errBagToStr b2
    (Left b1, Left b2) -> do dflags <- G.getSessionDynFlags
                             style <- toGhcModT getStyle
                             let merged = mergeErrors dflags style b1 b2
                             errBagToStr merged
  where
    setLogger logref df = Gap.setLogAction df $ appendLogRef df logref

----------------------------------------------------------------

-- | Converting 'SourceError' to 'String'.
sourceError :: IOish m => SourceError -> GhcModT m (Either String String)
sourceError err = errBagToStr (srcErrorMessages err)

errBagToStr :: IOish m => Bag ErrMsg -> GhcModT m (Either String String)
errBagToStr err = do
    dflags <- G.getSessionDynFlags
    style <- toGhcModT getStyle
    ret <- convert' (errBagToStrList dflags style err)
    return $ Left ret

errBagToStrList :: DynFlags -> PprStyle -> Bag ErrMsg -> [String]
errBagToStrList dflag style = map (ppErrMsg dflag style) . reverse . bagToList

sourceErrorBag :: IOish m => SourceError -> GhcModT m (Either (Bag ErrMsg) String)
sourceErrorBag err = return $ Left (srcErrorMessages err)

mergeErrors :: DynFlags -> PprStyle -> Bag ErrMsg -> Bag ErrMsg -> Bag ErrMsg
mergeErrors dflag style b1 b2 =
  let b1List = bagToList b1
      findInB1 = \pr2 msg2 err1 ->
                    let pr1  = ppMsgPrefix (Gap.errorMsgSpan err1) G.SevWarning dflag style
                        msg1 = showPage dflag style (errMsgExtraInfo err1)
                     in pr1 == pr2 && msg1 == msg2
      mustBeB2 = \err2 ->
                    let pr2  = ppMsgPrefix (Gap.errorMsgSpan err2) G.SevWarning dflag style
                        msg2 = showPage dflag style (errMsgExtraInfo err2)
                     in not . isJust $ find (findInB1 pr2 msg2) b1List
   in b1 `unionBags` filterBag mustBeB2 b2

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
    prefix = ppMsgPrefix spn sev dflag style

ppMsgPrefix :: SrcSpan -> Severity-> DynFlags -> PprStyle -> String
ppMsgPrefix spn sev dflag _style =
  let defaultPrefix
        | Gap.isDumpSplices dflag = ""
        | otherwise               = checkErrorPrefix
   in fromMaybe defaultPrefix $ do
        (line,col,_,_) <- Gap.getSrcSpan spn
        file <- normalise <$> Gap.getSrcFile spn
        let severityCaption = Gap.showSeverityCaption sev
        return $ file ++ ":" ++ show line ++ ":" ++ show col ++ ":" ++ severityCaption

checkErrorPrefix :: String
checkErrorPrefix = "Dummy:0:0:Error:"
