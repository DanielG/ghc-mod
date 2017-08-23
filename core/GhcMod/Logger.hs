{-# LANGUAGE CPP, RankNTypes #-}

module GhcMod.Logger (
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
import Control.Monad.Reader (Reader, ask, runReader)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import System.FilePath (normalise)

import ErrUtils
import GHC
import HscTypes
import Outputable
import qualified GHC as G
import Bag
import GhcMod.Convert
import GhcMod.Doc (showPage)
import GhcMod.DynFlags (withDynFlags)
import GhcMod.Monad.Types
import GhcMod.Error
import GhcMod.Pretty
import qualified GhcMod.Gap as Gap
import Prelude

type Builder = [String] -> [String]

data Log = Log [String] Builder

newtype LogRef = LogRef (IORef Log)

newtype GmPprEnv = GmPprEnv { gpeDynFlags :: DynFlags }

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

appendLogRef :: DynFlags -> LogRef -> Gap.GmLogAction
appendLogRef df (LogRef ref) _reason _df sev src st msg =
    modifyIORef ref update
  where
    -- TODO: get rid of ppMsg and just do more or less what ghc's
    -- defaultLogAction does
    l = ppMsg df st src sev msg

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
    logref <- liftIO newLogRef

    let setLogger df = Gap.setLogAction df $ appendLogRef df logref
        handlers = [
            GHandler $ \ex -> return $ Left $ runReader (sourceError ex) gpe,
            GHandler $ \ex -> return $ Left [renderGm $ ghcExceptionDoc ex]
          ]
        gpe = GmPprEnv {
                gpeDynFlags = hsc_dflags env
        }

    a <- gcatches (Right <$> action setLogger) handlers
    ls <- liftIO $ readAndClearLogRef logref

    return ((,) ls <$> a)

errBagToStrList :: (IOish m, GmState m, GmEnv m) => HscEnv -> Bag ErrMsg -> m [String]
errBagToStrList env errs =
   return $ runReader
    (errsToStr (sortMsgBag errs))
    GmPprEnv{ gpeDynFlags = hsc_dflags env }

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
    GmPprEnv {..} <- ask
    let unqual = errMsgContext err
        st = Gap.mkErrStyle' gpeDynFlags unqual
        err' = Gap.setErrorMsgSpan err $ Gap.errorMsgSpan err
    return $ showPage gpeDynFlags st $ pprLocErrMsg err'

ppMsg :: DynFlags -> PprStyle -> SrcSpan -> Severity -> SDoc -> String
ppMsg df st spn sev msg = let
    cts = showPage df st msg
  in
    ppMsgPrefix df spn sev cts ++ cts

ppMsgPrefix :: DynFlags -> SrcSpan -> Severity -> String -> String
ppMsgPrefix df spn sev cts =
  let
      defaultPrefix = if Gap.isDumpSplices df then "" else checkErrorPrefix
  in
    fromMaybe defaultPrefix $ do
      (line,col,_,_) <- Gap.getSrcSpan spn
      file <- normalise <$> Gap.getSrcFile spn
      return $ file ++ ":" ++ show line ++ ":" ++ show col ++ ":" ++
        if any (`isPrefixOf` cts) warningAsErrorPrefixes
          then ""
          else Gap.showSeverityCaption sev

checkErrorPrefix :: String
checkErrorPrefix = "Dummy:0:0:Error:"

warningAsErrorPrefixes :: [String]
warningAsErrorPrefixes = [ "Couldn't match expected type"
                         , "Couldn't match type"
                         , "No instance for"]
