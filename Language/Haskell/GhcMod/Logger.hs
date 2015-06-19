module Language.Haskell.GhcMod.Logger (
    withLogger
  , withLogger'
  , checkErrorPrefix
  , errsToStr
  , errBagToStrList
  ) where

import Control.Arrow
import Control.Applicative ((<$>))
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import System.FilePath (normalise)
import Text.PrettyPrint

import ErrUtils (ErrMsg, errMsgShortDoc, errMsgExtraInfo)
import GHC (DynFlags, SrcSpan, Severity(SevError))
import HscTypes
import Outputable
import qualified GHC as G
import Bag

import Language.Haskell.GhcMod.Convert
import Language.Haskell.GhcMod.Doc (showPage)
import Language.Haskell.GhcMod.DynFlags (withDynFlags)
import Language.Haskell.GhcMod.Monad.Types
import Language.Haskell.GhcMod.Error
import qualified Language.Haskell.GhcMod.Gap as Gap

type Builder = [String] -> [String]

data Log = Log [String] Builder

newtype LogRef = LogRef (IORef Log)

emptyLog :: Log
emptyLog = Log [] id

newLogRef :: IO LogRef
newLogRef = LogRef <$> newIORef emptyLog

readAndClearLogRef :: LogRef -> IO [String]
readAndClearLogRef (LogRef ref) = do
    Log _ b <- readIORef ref
    writeIORef ref emptyLog
    return $ b []

appendLogRef :: DynFlags -> LogRef -> DynFlags -> Severity -> SrcSpan -> PprStyle -> SDoc -> IO ()
appendLogRef df (LogRef ref) _ sev src st msg = modifyIORef ref update
  where
    l = ppMsg src sev df st msg
    update lg@(Log ls b)
      | l `elem` ls = lg
      | otherwise   = Log (l:ls) (b . (l:))

----------------------------------------------------------------

-- | Set the session flag (e.g. "-Wall" or "-w:") then
--   executes a body. Logged messages are returned as 'String'.
--   Right is success and Left is failure.
withLogger :: (GmGhc m, GmEnv m)
           => (DynFlags -> DynFlags)
           -> m a
           -> m (Either String (String, a))
withLogger f action = do
  env <- G.getSession
  opts <- options
  let conv = convert opts
  eres <- withLogger' env $ \setDf ->
      withDynFlags (f . setDf) action
  return $ either (Left . conv) (Right . first conv) eres

withLogger' :: IOish m
    => HscEnv -> ((DynFlags -> DynFlags) -> m a) -> m (Either [String] ([String], a))
withLogger' env action = do
    logref <- liftIO $ newLogRef

    let dflags = hsc_dflags env
        pu = icPrintUnqual dflags (hsc_IC env)
        st = mkUserStyle pu AllTheWay

        fn df  = setLogger logref df

    a <- gcatches (Right <$> action fn) (handlers dflags st)
    ls <- liftIO $ readAndClearLogRef logref

    return $ ((,) ls <$> a)

  where
    setLogger logref df = Gap.setLogAction df $ appendLogRef df logref
    handlers df st = [
        GHandler $ \ex -> return $ Left $ sourceError df st ex,
        GHandler $ \ex -> return $ Left [render $ ghcExceptionDoc ex]
     ]

errBagToStrList :: HscEnv -> Bag ErrMsg -> [String]
errBagToStrList env errs = let
    dflags = hsc_dflags env
    pu = icPrintUnqual dflags (hsc_IC env)
    st = mkUserStyle pu AllTheWay
 in errsToStr dflags st $ bagToList errs

----------------------------------------------------------------

-- | Converting 'SourceError' to 'String'.
sourceError :: DynFlags -> PprStyle -> SourceError -> [String]
sourceError df st src_err = errsToStr df st $ reverse $ bagToList $ srcErrorMessages src_err

errsToStr :: DynFlags -> PprStyle -> [ErrMsg] -> [String]
errsToStr df st = map (ppErrMsg df st)

----------------------------------------------------------------

ppErrMsg :: DynFlags -> PprStyle -> ErrMsg -> String
ppErrMsg dflag st err =
    ppMsg spn SevError dflag st msg ++ (if null ext then "" else "\n" ++ ext)
   where
     spn = Gap.errorMsgSpan err
     msg = errMsgShortDoc err
     ext = showPage dflag st (errMsgExtraInfo err)

ppMsg :: SrcSpan -> Severity-> DynFlags -> PprStyle -> SDoc -> String
ppMsg spn sev dflag st msg = prefix ++ cts
  where
    cts  = showPage dflag st msg
    prefix = ppMsgPrefix spn sev dflag st cts

ppMsgPrefix :: SrcSpan -> Severity-> DynFlags -> PprStyle -> String -> String
ppMsgPrefix spn sev dflag _st cts =
  let defaultPrefix
        | Gap.isDumpSplices dflag = ""
        | otherwise               = checkErrorPrefix
   in fromMaybe defaultPrefix $ do
        (line,col,_,_) <- Gap.getSrcSpan spn
        file <- normalise <$> Gap.getSrcFile spn
        let severityCaption = Gap.showSeverityCaption sev
            pref0 | or (map (\x -> x `isPrefixOf` cts) warningAsErrorPrefixes)
                              = file ++ ":" ++ show line ++ ":" ++ show col ++ ":"
                  | otherwise = file ++ ":" ++ show line ++ ":" ++ show col ++ ":" ++ severityCaption
        return pref0

checkErrorPrefix :: String
checkErrorPrefix = "Dummy:0:0:Error:"

warningAsErrorPrefixes :: [String]
warningAsErrorPrefixes = ["Couldn't match expected type"
                         , "Couldn't match type"
                         , "No instance for"]
