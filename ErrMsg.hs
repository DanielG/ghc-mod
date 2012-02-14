{-# LANGUAGE CPP #-}

module ErrMsg (
    LogReader
  , setLogger
  , handleErrMsg
  ) where

import Bag
import Control.Applicative
import Data.IORef
import DynFlags
import ErrUtils
import FastString
import GHC
import HscTypes
import Outputable

#if __GLASGOW_HASKELL__ < 702
import Pretty
#endif

----------------------------------------------------------------

type LogReader = IO [String]

----------------------------------------------------------------

setLogger :: Bool -> DynFlags -> IO (DynFlags, LogReader)
setLogger False df = return (newdf, undefined)
  where
    newdf = df { log_action = \_ _ _ _ -> return () }
setLogger True  df = do
    ref <- newIORef [] :: IO (IORef [String])
    let newdf = df { log_action = appendLog ref }
    return (newdf, reverse <$> readIORef ref)
  where
    appendLog ref _ src stl msg = modifyIORef ref (\ls -> ppMsg src msg stl : ls)

----------------------------------------------------------------

handleErrMsg :: SourceError -> Ghc [String]
handleErrMsg = return . errBagToStrList . srcErrorMessages

errBagToStrList :: Bag ErrMsg -> [String]
errBagToStrList = map ppErrMsg . reverse . bagToList

----------------------------------------------------------------

ppErrMsg :: ErrMsg -> String
ppErrMsg err = ppMsg spn msg defaultUserStyle ++ ext
   where
     spn = head (errMsgSpans err)
     msg = errMsgShortDoc err
     ext = showMsg (errMsgExtraInfo err) defaultUserStyle

ppMsg :: SrcSpan -> Message -> PprStyle -> String
#if __GLASGOW_HASKELL__ >= 702
ppMsg (RealSrcSpan src) msg stl
#else
ppMsg src msg stl | isGoodSrcSpan src
#endif
    = file ++ ":" ++ line ++ ":" ++ col ++ ":" ++ cts ++ "\0"
  where
    file = unpackFS (srcSpanFile src)
    line = show (srcSpanStartLine src)
    col  = show (srcSpanStartCol src)
    cts  = showMsg msg stl
ppMsg _ _ _ = "ghc-mod:0:0:Probably mutual module import occurred\0"

----------------------------------------------------------------

showMsg :: SDoc -> PprStyle -> String
#if __GLASGOW_HASKELL__ >= 702
showMsg d stl = map toNull . renderWithStyle d $ stl
#else
showMsg d stl = map toNull . Pretty.showDocWith PageMode $ d stl
#endif
  where
    toNull '\n' = '\0'
    toNull x = x
