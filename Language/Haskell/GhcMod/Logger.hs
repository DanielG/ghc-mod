module Language.Haskell.GhcMod.Logger (
    withLogger
  , withLogger'
  , checkErrorPrefix
  , errsToStr
  , errBagToStrList
  ) where

import Control.Arrow
import Control.Applicative
import Data.Ord
import Data.List
import Data.Maybe
import Data.Function
import Control.Monad.Reader (Reader, asks, runReader)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import System.FilePath (normalise)
import Text.PrettyPrint

import ErrUtils
import GHC
import HscTypes
import Outputable
import qualified GHC as G
import Bag

import Language.Haskell.GhcMod.Convert
import Language.Haskell.GhcMod.Doc (showPage)
import Language.Haskell.GhcMod.DynFlags (withDynFlags)
import Language.Haskell.GhcMod.Monad.Types
import Language.Haskell.GhcMod.Error
import Language.Haskell.GhcMod.Utils (mkRevRedirMapFunc)
import qualified Language.Haskell.GhcMod.Gap as Gap
import Prelude

type Builder = [String] -> [String]

data Log = Log [String] Builder

newtype LogRef = LogRef (IORef Log)

data GmPprEnv = GmPprEnv { gpeDynFlags :: DynFlags
                         , gpeMapFile :: FilePath -> FilePath
                         }

type GmPprEnvM a = Reader GmPprEnv a

emptyLog :: Log
emptyLog = Log [] id

newLogRef :: IO LogRef
newLogRef = LogRef <$> newIORef emptyLog

readAndClearLogRef :: LogRef -> IO [String]
readAndClearLogRef (LogRef ref) = do
    Log _ b <- readIORef ref
    writeIORef ref emptyLog
    return $ b []

appendLogRef :: (FilePath -> FilePath) -> DynFlags -> LogRef -> DynFlags -> Severity -> SrcSpan -> PprStyle -> SDoc -> IO ()
appendLogRef rfm df (LogRef ref) _ sev src st msg = do
    modifyIORef ref update
  where
    gpe = GmPprEnv {
            gpeDynFlags = df
          , gpeMapFile = rfm
          }
    l = runReader (ppMsg st src sev msg) gpe

    update lg@(Log ls b)
      | l `elem` ls = lg
      | otherwise   = Log (l:ls) (b . (l:))

----------------------------------------------------------------

-- | Logged messages are returned as 'String'.
--   Right is success and Left is failure.
withLogger :: (GmGhc m, GmEnv m, GmOut m, GmState m)
           => (DynFlags -> DynFlags)
           -> m a
           -> m (Either String (String, a))
withLogger f action = do
  env <- G.getSession
  oopts <- outputOpts
  let conv = convert oopts
  eres <- withLogger' env $ \setDf ->
      withDynFlags (f . setDf) action
  return $ either (Left . conv) (Right . first conv) eres

withLogger' :: (IOish m, GmState m, GmEnv m)
    => HscEnv -> ((DynFlags -> DynFlags) -> m a) -> m (Either [String] ([String], a))
withLogger' env action = do
    logref <- liftIO $ newLogRef

    rfm <- mkRevRedirMapFunc

    let setLogger df = Gap.setLogAction df $ appendLogRef rfm df logref
        handlers = [
            GHandler $ \ex -> return $ Left $ runReader (sourceError ex) gpe,
            GHandler $ \ex -> return $ Left [render $ ghcExceptionDoc ex]
          ]
        gpe = GmPprEnv {
                gpeDynFlags = hsc_dflags env
              , gpeMapFile = rfm
        }

    a <- gcatches (Right <$> action setLogger) handlers
    ls <- liftIO $ readAndClearLogRef logref

    return ((,) ls <$> a)

errBagToStrList :: (IOish m, GmState m, GmEnv m) => HscEnv -> Bag ErrMsg -> m [String]
errBagToStrList env errs = do
   rfm <- mkRevRedirMapFunc
   return $ runReader
    (errsToStr (sortMsgBag errs))
    GmPprEnv{ gpeDynFlags = hsc_dflags env, gpeMapFile = rfm }

----------------------------------------------------------------

-- | Converting 'SourceError' to 'String'.
sourceError :: SourceError -> GmPprEnvM [String]
sourceError = errsToStr . sortMsgBag . srcErrorMessages

errsToStr :: [ErrMsg] -> GmPprEnvM [String]
errsToStr = mapM ppErrMsg

sortMsgBag :: Bag ErrMsg -> [ErrMsg]
sortMsgBag bag = sortBy (compare `on` Gap.errorMsgSpan) $ bagToList bag

----------------------------------------------------------------

ppErrMsg :: ErrMsg -> GmPprEnvM String
ppErrMsg err = do
    dflags <- asks gpeDynFlags
    let unqual = errMsgContext err
        st = Gap.mkErrStyle' dflags unqual
    let ext = showPage dflags st (errMsgExtraInfo err)
    m <- ppMsg st spn SevError msg
    return $ m ++ (if null ext then "" else "\n" ++ ext)
   where
     spn = Gap.errorMsgSpan err
     msg = errMsgShortDoc err

ppMsg :: PprStyle -> SrcSpan -> Severity -> SDoc -> GmPprEnvM String
ppMsg st spn sev msg = do
  dflags <- asks gpeDynFlags
  let cts  = showPage dflags st msg
  prefix <- ppMsgPrefix spn sev cts
  return $ prefix ++ cts

ppMsgPrefix :: SrcSpan -> Severity -> String -> GmPprEnvM String
ppMsgPrefix spn sev cts = do
  dflags <- asks gpeDynFlags
  mr <- asks gpeMapFile
  let defaultPrefix
        | Gap.isDumpSplices dflags = ""
        | otherwise               = checkErrorPrefix
  return $ fromMaybe defaultPrefix $ do
    (line,col,_,_) <- Gap.getSrcSpan spn
    file <- mr <$> normalise <$> Gap.getSrcFile spn
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
