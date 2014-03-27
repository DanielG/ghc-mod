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
import Language.Haskell.GhcMod.Doc (showUnqualifiedPage)
import qualified Language.Haskell.GhcMod.Gap as Gap
import Language.Haskell.GhcMod.Types (LineSeparator(..))
import Outputable (SDoc)
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

appendLogRef :: DynFlags -> LineSeparator -> LogRef -> a -> Severity -> SrcSpan -> b -> SDoc -> IO ()
appendLogRef df ls (LogRef ref) _ sev src _ msg = do
        let !l = ppMsg src sev df ls msg
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
    return . errBagToStrList dflag ls . srcErrorMessages $ err

errBagToStrList :: DynFlags -> LineSeparator -> Bag ErrMsg -> [String]
errBagToStrList dflag ls = map (ppErrMsg dflag ls) . reverse . bagToList

----------------------------------------------------------------

ppErrMsg :: DynFlags -> LineSeparator -> ErrMsg -> String
ppErrMsg dflag ls err = ppMsg spn SevError dflag ls msg ++ ext
   where
     spn = Gap.errorMsgSpan err
     msg = errMsgShortDoc err
     ext = showMsg dflag ls (errMsgExtraInfo err)

ppMsg :: SrcSpan -> Severity-> DynFlags -> LineSeparator -> SDoc -> String
ppMsg spn sev dflag ls msg = prefix ++ cts
  where
    cts  = showMsg dflag ls msg
    defaultPrefix
      | dopt Gap.dumpSplicesFlag dflag = ""
      | otherwise                      = "Dummy:0:0:Error:"
    prefix = fromMaybe defaultPrefix $ do
        (line,col,_,_) <- Gap.getSrcSpan spn
        file <- normalise <$> Gap.getSrcFile spn
        let severityCaption = Gap.showSeverityCaption sev
        return $ file ++ ":" ++ show line ++ ":" ++ show col ++ ":" ++ severityCaption

----------------------------------------------------------------

showMsg :: DynFlags -> LineSeparator -> SDoc -> String
showMsg dflag (LineSeparator lsep) sdoc = replaceNull $ showUnqualifiedPage dflag sdoc
  where
    replaceNull []        = []
    replaceNull ('\n':xs) = lsep ++ replaceNull xs
    replaceNull (x:xs)    = x : replaceNull xs
