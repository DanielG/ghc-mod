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
import Language.Haskell.GhcMod.Types (LineSeparator(..))
import qualified Language.Haskell.GhcMod.Gap as Gap
import Outputable
import System.FilePath (normalise)

----------------------------------------------------------------

-- | A means to read the log
type LogReader = IO [String]

----------------------------------------------------------------

setLogger :: Bool -> DynFlags -> LineSeparator -> IO (DynFlags, LogReader)
setLogger False df _ = return (newdf, undefined)
  where
    newdf = Gap.setLogAction df $ \_ _ _ _ _ -> return ()
setLogger True  df ls = do
    ref <- newIORef [] :: IO (IORef [String])
    let newdf = Gap.setLogAction df $ appendLog ref
    return (newdf, reverse <$> readIORef ref)
  where
    appendLog ref _ sev src _ msg = do
        let !l = ppMsg src sev df ls msg
        modifyIORef ref (l:)

----------------------------------------------------------------

handleErrMsg :: LineSeparator -> SourceError -> Ghc [String]
handleErrMsg ls err = do
    dflag <- getSessionDynFlags
    return . errBagToStrList dflag ls . srcErrorMessages $ err

errBagToStrList :: DynFlags -> LineSeparator -> Bag ErrMsg -> [String]
errBagToStrList dflag ls = map (ppErrMsg dflag ls) . reverse . bagToList

----------------------------------------------------------------

ppErrMsg :: DynFlags -> LineSeparator -> ErrMsg -> String
ppErrMsg dflag ls err = ppMsg spn SevError dflag ls msg ++ ext
   where
     spn = head (errMsgSpans err)
     msg = errMsgShortDoc err
     ext = showMsg dflag ls (errMsgExtraInfo err)

ppMsg :: SrcSpan -> Severity-> DynFlags -> LineSeparator -> SDoc -> String
ppMsg spn sev dflag ls@(LineSeparator lsep) msg = prefix ++ cts ++ lsep
  where
    cts  = showMsg dflag ls msg
    defaultPrefix
      | dopt Opt_D_dump_splices dflag = ""
      | otherwise                     = "Dummy:0:0:"
    prefix = fromMaybe defaultPrefix $ do
        (line,col,_,_) <- Gap.getSrcSpan spn
        file <- normalise <$> Gap.getSrcFile spn
        let severityCaption = Gap.showSeverityCaption sev
        return $ file ++ ":" ++ show line ++ ":" ++ show col ++ ":" ++ severityCaption

----------------------------------------------------------------

showMsg :: DynFlags -> LineSeparator -> SDoc -> String
showMsg dflag (LineSeparator [s]) sdoc = replaceNull $ showUnqualifiedPage dflag sdoc
  where
    replaceNull :: String -> String
    replaceNull []        = []
    replaceNull ('\n':xs) = s : replaceNull xs
    replaceNull (x:xs)    = x : replaceNull xs
showMsg dflag (LineSeparator lsep) sdoc = replaceNull $ showUnqualifiedPage dflag sdoc
  where
    replaceNull []        = []
    replaceNull ('\n':xs) = lsep ++ replaceNull xs
    replaceNull (x:xs)    = x : replaceNull xs
