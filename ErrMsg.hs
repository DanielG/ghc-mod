module ErrMsg (
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
import qualified Gap
import HscTypes
import Outputable

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
ppMsg spn msg stl = fromMaybe def $ do
    (line,col,_,_) <- Gap.getSrcSpan spn
    file <- Gap.getSrcFile spn
    return $ file ++ ":" ++ show line ++ ":" ++ show col ++ ":" ++ cts ++ "\0"
  where
    def = "ghc-mod:0:0:Probably mutual module import occurred\0"
    cts  = showMsg msg stl

----------------------------------------------------------------

showMsg :: SDoc -> PprStyle -> String
showMsg d stl = map toNull $ Gap.renderMsg d stl
  where
    toNull '\n' = '\0'
    toNull x = x
