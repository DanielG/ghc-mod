{-# LANGUAGE BangPatterns, CPP #-}

module Language.Haskell.GhcMod.ErrMsg (
    LogReader
  , setLogger
  , handleErrMsg
  ) where

import Bag (Bag, bagToList)
import Control.Applicative ((<$>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.Maybe (fromMaybe)
import DynFlags (dopt)
import ErrUtils (ErrMsg, errMsgShortDoc, errMsgExtraInfo)
import GHC (Ghc, DynFlags, SrcSpan, Severity(SevError))
import qualified GHC as G
import HscTypes (SourceError, srcErrorMessages)
import Language.Haskell.GhcMod.Doc (showPage, getStyle)
import qualified Language.Haskell.GhcMod.Gap as Gap
import Language.Haskell.GhcMod.Types (LineSeparator(..), Options(..), convert)
import Language.Haskell.GhcMod.Utils (replace)
import Outputable (PprStyle, SDoc)
import System.FilePath (normalise)

----------------------------------------------------------------

-- | A means to read the log.
type LogReader = IO String

----------------------------------------------------------------

type Builder = [String] -> [String]

newtype LogRef = LogRef (IORef Builder)

newLogRef :: IO LogRef
newLogRef = LogRef <$> newIORef id

readAndClearLogRef :: Options -> LogRef -> IO String
readAndClearLogRef opt (LogRef ref) = do
    b <- readIORef ref
    writeIORef ref id
    return $! convert opt (b [])

appendLogRef :: DynFlags -> Options -> LogRef -> DynFlags -> Severity -> SrcSpan -> PprStyle -> SDoc -> IO ()
appendLogRef df opt (LogRef ref) _ sev src style msg = do
        let !l = ppMsg src sev df opt style msg
        modifyIORef ref (\b -> b . (l:))

----------------------------------------------------------------

setLogger :: Bool -> DynFlags -> Options -> IO (DynFlags, LogReader)
setLogger False df _ = return (newdf, undefined)
  where
    newdf = Gap.setLogAction df $ \_ _ _ _ _ -> return ()
setLogger True  df opt = do
    logref <- newLogRef
    let newdf = Gap.setLogAction df $ appendLogRef df opt logref
    return (newdf, readAndClearLogRef opt logref)

----------------------------------------------------------------

-- | Converting 'SourceError' to 'String'.
handleErrMsg :: Options -> SourceError -> Ghc String
handleErrMsg opt err = do
    dflag <- G.getSessionDynFlags
    style <- getStyle
    let ret = convert opt . errBagToStrList dflag opt style . srcErrorMessages $ err
    return ret

errBagToStrList :: DynFlags -> Options -> PprStyle -> Bag ErrMsg -> [String]
errBagToStrList dflag opt style = map (ppErrMsg dflag opt style) . reverse . bagToList

----------------------------------------------------------------

ppErrMsg :: DynFlags -> Options -> PprStyle -> ErrMsg -> String
ppErrMsg dflag opt style err = ppMsg spn SevError dflag opt style msg ++ ext
   where
     spn = Gap.errorMsgSpan err
     msg = errMsgShortDoc err
     ext = showMsg dflag opt style (errMsgExtraInfo err)

ppMsg :: SrcSpan -> Severity-> DynFlags -> Options -> PprStyle -> SDoc -> String
ppMsg spn sev dflag opt style msg = prefix ++ cts
  where
    cts  = showMsg dflag opt style msg
    defaultPrefix
      | dopt Gap.dumpSplicesFlag dflag = ""
      | otherwise                      = "Dummy:0:0:Error:"
    prefix = fromMaybe defaultPrefix $ do
        (line,col,_,_) <- Gap.getSrcSpan spn
        file <- normalise <$> Gap.getSrcFile spn
        let severityCaption = Gap.showSeverityCaption sev
        return $ file ++ ":" ++ show line ++ ":" ++ show col ++ ":" ++ severityCaption

----------------------------------------------------------------

showMsg :: DynFlags -> Options -> PprStyle -> SDoc -> String
showMsg dflag opt style sdoc = replace '\n' lsep $ showPage dflag style sdoc
  where
    LineSeparator lsep = lineSeparator opt
