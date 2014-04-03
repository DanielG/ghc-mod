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
import Language.Haskell.GhcMod.Types (LineSeparator(..))
import Outputable (PprStyle, SDoc)
import System.FilePath (normalise)

----------------------------------------------------------------

-- | A means to read the log.
type LogReader = IO [String]

----------------------------------------------------------------

type Builder = [String] -> [String]

newtype LogRef = LogRef (IORef Builder)

newLogRef :: IO LogRef
newLogRef = LogRef <$> newIORef id

readAndClearLogRef :: LogRef -> IO [String]
readAndClearLogRef (LogRef ref) = do
    b <- readIORef ref
    writeIORef ref id
    return $! b []

appendLogRef :: DynFlags -> LineSeparator -> LogRef -> DynFlags -> Severity -> SrcSpan -> PprStyle -> SDoc -> IO ()
appendLogRef df ls (LogRef ref) _ sev src style msg = do
        let !l = ppMsg src sev df ls style msg
        modifyIORef ref (\b -> b . (l:))

----------------------------------------------------------------

setLogger :: Bool -> DynFlags -> LineSeparator -> IO (DynFlags, LogReader)
setLogger False df _ = return (newdf, undefined)
  where
    newdf = Gap.setLogAction df $ \_ _ _ _ _ -> return ()
setLogger True  df ls = do
    logref <- newLogRef
    let newdf = Gap.setLogAction df $ appendLogRef df ls logref
    return (newdf, readAndClearLogRef logref)

----------------------------------------------------------------

-- | Converting 'SourceError' to 'String'.
handleErrMsg :: LineSeparator -> SourceError -> Ghc [String]
handleErrMsg ls err = do
    dflag <- G.getSessionDynFlags
    style <- getStyle
    return . errBagToStrList dflag ls style . srcErrorMessages $ err

errBagToStrList :: DynFlags -> LineSeparator -> PprStyle -> Bag ErrMsg -> [String]
errBagToStrList dflag ls style = map (ppErrMsg dflag ls style) . reverse . bagToList

----------------------------------------------------------------

ppErrMsg :: DynFlags -> LineSeparator -> PprStyle -> ErrMsg -> String
ppErrMsg dflag ls style err = ppMsg spn SevError dflag ls style msg ++ ext
   where
     spn = Gap.errorMsgSpan err
     msg = errMsgShortDoc err
     ext = showMsg dflag ls style (errMsgExtraInfo err)

ppMsg :: SrcSpan -> Severity-> DynFlags -> LineSeparator -> PprStyle -> SDoc -> String
ppMsg spn sev dflag ls style msg = prefix ++ cts
  where
    cts  = showMsg dflag ls style msg
    defaultPrefix
      | dopt Gap.dumpSplicesFlag dflag = ""
      | otherwise                      = "Dummy:0:0:Error:"
    prefix = fromMaybe defaultPrefix $ do
        (line,col,_,_) <- Gap.getSrcSpan spn
        file <- normalise <$> Gap.getSrcFile spn
        let severityCaption = Gap.showSeverityCaption sev
        return $ file ++ ":" ++ show line ++ ":" ++ show col ++ ":" ++ severityCaption

----------------------------------------------------------------

showMsg :: DynFlags -> LineSeparator -> PprStyle -> SDoc -> String
showMsg dflag (LineSeparator lsep) style sdoc = replaceNull $ showPage dflag style sdoc
  where
    replaceNull []        = []
    replaceNull ('\n':xs) = lsep ++ replaceNull xs
    replaceNull (x:xs)    = x : replaceNull xs
