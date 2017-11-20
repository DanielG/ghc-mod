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
import SrcLoc
import FastString

import GhcMod.Convert
import GhcMod.Doc (showPage)
import GhcMod.DynFlags (withDynFlags)
import GhcMod.Monad.Types
import GhcMod.Error
import GhcMod.Pretty
import GhcMod.Utils (mkRevRedirMapFunc)
import qualified GhcMod.Gap as Gap
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

appendLogRef :: (FilePath -> FilePath) -> DynFlags -> LogRef -> Gap.GmLogAction
appendLogRef map_file df (LogRef ref) _reason _df sev src st msg = do
    modifyIORef ref update
  where
    -- TODO: get rid of ppMsg and just do more or less what ghc's
    -- defaultLogAction does
    l = ppMsg map_file df st src sev msg

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
            GHandler $ \ex -> return $ Left [renderGm $ ghcExceptionDoc ex]
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
    GmPprEnv {..} <- ask
    let unqual = errMsgContext err
        st = Gap.mkErrStyle' gpeDynFlags unqual
        err' = Gap.setErrorMsgSpan err $ mapSrcSpanFile gpeMapFile (Gap.errorMsgSpan err)
    return $ showPage gpeDynFlags st $ pprLocErrMsg err'

mapSrcSpanFile :: (FilePath -> FilePath) -> SrcSpan -> SrcSpan
mapSrcSpanFile map_file (RealSrcSpan s)   =
    RealSrcSpan $ mapRealSrcSpanFile map_file s
mapSrcSpanFile _ (UnhelpfulSpan s) =
    UnhelpfulSpan s

mapRealSrcSpanFile :: (FilePath -> FilePath) -> RealSrcSpan -> RealSrcSpan
mapRealSrcSpanFile map_file s = let
    start = mapRealSrcLocFile map_file $ realSrcSpanStart s
    end   = mapRealSrcLocFile map_file $ realSrcSpanEnd s
  in
    mkRealSrcSpan start end

mapRealSrcLocFile :: (FilePath -> FilePath) -> RealSrcLoc -> RealSrcLoc
mapRealSrcLocFile map_file l = let
    file = mkFastString $ map_file $ unpackFS $ srcLocFile l
    line = srcLocLine l
    col  = srcLocCol l
  in
    mkRealSrcLoc file line col

ppMsg :: (FilePath -> FilePath) -> DynFlags -> PprStyle -> SrcSpan -> Severity -> SDoc -> String
ppMsg map_file df st spn sev msg = let
    cts = showPage df st msg
  in
    ppMsgPrefix map_file df spn sev cts ++ cts

ppMsgPrefix :: (FilePath -> FilePath) -> DynFlags -> SrcSpan -> Severity -> String -> String
ppMsgPrefix map_file df spn sev cts =
  let
      defaultPrefix = if Gap.isDumpSplices df then "" else checkErrorPrefix
  in
    fromMaybe defaultPrefix $ do
      (line,col,_,_) <- Gap.getSrcSpan spn
      file <- map_file <$> normalise <$> Gap.getSrcFile spn
      return $ file ++ ":" ++ show line ++ ":" ++ show col ++ ":" ++
        if or (map (\x -> x `isPrefixOf` cts) warningAsErrorPrefixes)
          then ""
          else Gap.showSeverityCaption sev

checkErrorPrefix :: String
checkErrorPrefix = "Dummy:0:0:Error:"

warningAsErrorPrefixes :: [String]
warningAsErrorPrefixes = [ "Couldn't match expected type"
                         , "Couldn't match type"
                         , "No instance for"]
