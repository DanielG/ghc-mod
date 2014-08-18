{-# LANGUAGE CPP #-}

module Language.Haskell.GhcMod.Logger (
    withLogger
  , withLoggerTwice
  , checkErrorPrefix
  ) where

import Bag (Bag, bagToList, emptyBag, consBag, filterBag, unionBags)
import Control.Applicative ((<$>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.List (isPrefixOf, find, nub, isInfixOf)
import Data.Maybe (fromMaybe, isJust)
import ErrUtils (ErrMsg, WarnMsg, errMsgShortDoc, errMsgExtraInfo, mkWarnMsg)
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
import Outputable (PprStyle, SDoc, qualName, qualModule, mkErrStyle, neverQualify)
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

data LogBag = LogBag (Bag WarnMsg)
newtype LogBagRef = LogBagRef (IORef LogBag)

emptyLogBag :: LogBag
emptyLogBag = LogBag emptyBag

newLogBagRef :: IO LogBagRef
newLogBagRef = LogBagRef <$> newIORef emptyLogBag

readAndClearLogBagRef :: IOish m => LogBagRef -> GhcModT m (Bag WarnMsg)
readAndClearLogBagRef (LogBagRef ref) = do
    LogBag b <- liftIO $ readIORef ref
    liftIO $ writeIORef ref emptyLogBag
    return b

appendLogBagRef :: DynFlags -> LogBagRef -> DynFlags -> Severity -> SrcSpan -> PprStyle -> SDoc -> IO ()
appendLogBagRef df (LogBagRef ref) _ _ src style msg = modifyIORef ref update
  where
    qstyle = (qualName style, qualModule style)
#if __GLASGOW_HASKELL__ >= 706
    warnMsg = mkWarnMsg df src qstyle msg
#else
    warnMsg = mkWarnMsg src qstyle msg
#endif
    warnBag = consBag warnMsg emptyBag
    update (LogBag b) = let (b1,b2) = mergeErrors df style b warnBag
                         in LogBag $ b1 `unionBags` b2

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
    logref <- liftIO newLogBagRef
    wflags <- filter ("-fno-warn" `isPrefixOf`) . ghcUserOptions <$> options
    withDynFlags (setLogger logref . setDF1) $
        withCmdFlags wflags $ do
            body1
            Right <$> readAndClearLogBagRef logref
  err2 <- ghandle sourceErrorBag $ do
    logref <- liftIO newLogBagRef
    wflags <- filter ("-fno-warn" `isPrefixOf`) . ghcUserOptions <$> options
    withDynFlags (setLogger logref . setDF2) $
        withCmdFlags wflags $ do
            body2
            Right <$> readAndClearLogBagRef logref
  -- Merge errors and warnings
  dflags <- G.getSessionDynFlags
  style <- getStyle
  case (err1, err2) of
    (Right b1, Right b2) -> do let (warn1,_) = mergeErrors dflags style b1 b2
                               errAndWarnBagToStr Right emptyBag (warn1 `unionBags` b2)
    (Left  b1, Right b2) -> do let (err,warn) = mergeErrors dflags style b1 b2
                               errAndWarnBagToStr Right err warn
    (Right b1, Left  b2) -> do let (err,warn) = mergeErrors dflags style b2 b1
                               errAndWarnBagToStr Right err warn
    (Left  b1, Left  b2) -> do let (err1',err2') = mergeErrors dflags style b1 b2
                               errAndWarnBagToStr Right (err1' `unionBags` err2') emptyBag
  where
    setLogger logref df = Gap.setLogAction df $ appendLogBagRef df logref

----------------------------------------------------------------

-- | Converting 'SourceError' to 'String'.
sourceError :: IOish m => SourceError -> GhcModT m (Either String String)
sourceError err = errBagToStr (srcErrorMessages err)

errBagToStr :: IOish m => Bag ErrMsg -> GhcModT m (Either String String)
errBagToStr = errBagToStr' Left

errBagToStr' :: IOish m => (String -> a) -> Bag ErrMsg -> GhcModT m a
errBagToStr' f err = do
    dflags <- G.getSessionDynFlags
    style <- getStyle
    ret <- convert' (errBagToStrList dflags style err)
    return $ f ret

errAndWarnBagToStr :: IOish m => (String -> a) -> Bag ErrMsg -> Bag WarnMsg -> GhcModT m a
errAndWarnBagToStr f err warn = do
    dflags <- G.getSessionDynFlags
    -- style <- toGhcModT getStyle
#if __GLASGOW_HASKELL__ >= 706
    let style = mkErrStyle dflags neverQualify
#else
    let style = mkErrStyle neverQualify
#endif
    ret <- convert' $ nub (errBagToStrList dflags style err ++ warnBagToStrList dflags style warn)
    return $ f ret

errBagToStrList :: DynFlags -> PprStyle -> Bag ErrMsg -> [String]
errBagToStrList dflag style = map (ppErrMsg dflag style) . reverse . bagToList

warnBagToStrList :: DynFlags -> PprStyle -> Bag WarnMsg -> [String]
warnBagToStrList dflag style = map (ppWarnMsg dflag style) . reverse . bagToList

sourceErrorBag :: IOish m => SourceError -> GhcModT m (Either (Bag ErrMsg) (Bag WarnMsg))
sourceErrorBag err = return $ Left (srcErrorMessages err)

mergeErrors :: DynFlags -> PprStyle -> Bag ErrMsg -> Bag ErrMsg -> (Bag ErrMsg, Bag ErrMsg)
mergeErrors dflag style b1 b2 =
  let b1Msgs = map (\err1 -> let m = ppWarnMsg dflag style err1 in (m, head $ lines m))
                   (bagToList b1)
      mustBeB2 = \err2 -> let msg2  = ppWarnMsg dflag style err2
                              line2 = head $ lines msg2
                           in not . isJust $ find (\(msg1, line1) -> msg1 == msg2 || (line1 == line2 && isHoleMsg line1)) b1Msgs
   in (b1, filterBag mustBeB2 b2)

isHoleMsg :: String -> Bool
isHoleMsg = isInfixOf "Found hole"

----------------------------------------------------------------

ppErrMsg :: DynFlags -> PprStyle -> ErrMsg -> String
ppErrMsg dflag style err = ppMsg spn SevError dflag style msg ++ (if null ext then "" else "\n" ++ ext)
   where
     spn = Gap.errorMsgSpan err
     msg = errMsgShortDoc err
     ext = showPage dflag style (errMsgExtraInfo err)

ppWarnMsg :: DynFlags -> PprStyle -> ErrMsg -> String
ppWarnMsg dflag style err = ppMsg spn G.SevWarning dflag style msg ++ (if null ext then "" else "\n" ++ ext)
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
