{-# LANGUAGE BangPatterns #-}

module Language.Haskell.GhcMod.ErrMsg (
    LogReader
  , setLogger
  , handleErrMsg
  ) where

import Bag
import Control.Applicative
import Data.IORef
import Data.Maybe
import DynFlags
import ErrUtils
import GHC
import HscTypes
import Language.Haskell.GhcMod.Doc (showUnqualifiedPage)
import qualified Language.Haskell.GhcMod.Gap as Gap
import Outputable
import System.FilePath (normalise)

----------------------------------------------------------------

-- | A means to read the log
type LogReader = IO [String]

----------------------------------------------------------------

setLogger :: Bool -> DynFlags -> IO (DynFlags, LogReader)
setLogger False df = return (newdf, undefined)
  where
    newdf = Gap.setLogAction df $ \_ _ _ _ _ -> return ()
setLogger True  df = do
    ref <- newIORef [] :: IO (IORef [String])
    let newdf = Gap.setLogAction df $ appendLog ref
    return (newdf, reverse <$> readIORef ref)
  where
    appendLog ref _ sev src _ msg = do
        let !l = ppMsg src sev df msg
        modifyIORef ref (\ls -> l : ls)

----------------------------------------------------------------

handleErrMsg :: SourceError -> Ghc [String]
handleErrMsg err = do
    dflag <- getSessionDynFlags
    return . errBagToStrList dflag . srcErrorMessages $ err

errBagToStrList :: DynFlags -> Bag ErrMsg -> [String]
errBagToStrList dflag = map (ppErrMsg dflag) . reverse . bagToList

----------------------------------------------------------------

ppErrMsg :: DynFlags -> ErrMsg -> String
ppErrMsg dflag err = ppMsg spn SevError dflag msg ++ ext
   where
     spn = head (errMsgSpans err)
     msg = errMsgShortDoc err
     ext = showMsg dflag (errMsgExtraInfo err)

ppMsg :: SrcSpan -> Severity-> DynFlags -> SDoc -> String
ppMsg spn sev dflag msg = prefix ++ cts ++ "\0"
  where
    cts  = showMsg dflag msg
    defaultPrefix
      | dopt Opt_D_dump_splices dflag = ""
      | otherwise                     = "Dummy:0:0:"
    prefix = fromMaybe defaultPrefix $ do
        (line,col,_,_) <- Gap.getSrcSpan spn
        file <- normalise <$> Gap.getSrcFile spn
        let severityCaption = Gap.showSeverityCaption sev
        return $ file ++ ":" ++ show line ++ ":" ++ show col ++ ":" ++ severityCaption

----------------------------------------------------------------

showMsg :: DynFlags -> SDoc -> String
showMsg dflag sdoc = map toNull $ showUnqualifiedPage dflag sdoc
  where
    toNull '\n' = '\0'
    toNull x = x
