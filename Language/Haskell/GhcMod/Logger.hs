{-# LANGUAGE BangPatterns, CPP #-}

module Language.Haskell.GhcMod.Logger (
    withLogger
  , checkErrorPrefix
  ) where

import Bag (Bag, bagToList)
import Control.Applicative ((<$>))
import CoreMonad (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.Maybe (fromMaybe)
import ErrUtils (ErrMsg, errMsgShortDoc, errMsgExtraInfo)
import Exception (ghandle)
import GHC (Ghc, DynFlags, SrcSpan, Severity(SevError))
import qualified GHC as G
import HscTypes (SourceError, srcErrorMessages)
import Language.Haskell.GhcMod.Doc (showPage, getStyle)
import Language.Haskell.GhcMod.GHCApi (withDynFlags)
import qualified Language.Haskell.GhcMod.Gap as Gap
import Language.Haskell.GhcMod.Types (Options, convert)
import Outputable (PprStyle, SDoc)
import System.FilePath (normalise)

----------------------------------------------------------------

type Builder = [String] -> [String]

newtype LogRef = LogRef (IORef Builder)

newLogRef :: IO LogRef
newLogRef = LogRef <$> newIORef id

readAndClearLogRef :: Options -> LogRef -> IO String
readAndClearLogRef opt (LogRef ref) = do
    b <- readIORef ref
    writeIORef ref id
    return $! convert opt (b [])

appendLogRef :: DynFlags -> LogRef -> DynFlags -> Severity -> SrcSpan -> PprStyle -> SDoc -> IO ()
appendLogRef df (LogRef ref) _ sev src style msg = do
        let !l = ppMsg src sev df style msg
        modifyIORef ref (\b -> b . (l:))

----------------------------------------------------------------

withLogger :: Options -> (DynFlags -> DynFlags) -> Ghc () -> Ghc (Either String String)
withLogger opt setDF body = ghandle (handleErrMsg opt) $ do
    logref <- liftIO $ newLogRef
    withDynFlags (setLogger logref . setDF) $ do
        body
        liftIO $ Right <$> readAndClearLogRef opt logref
  where
    setLogger logref df = Gap.setLogAction df $ appendLogRef df logref

----------------------------------------------------------------

-- | Converting 'SourceError' to 'String'.
handleErrMsg :: Options -> SourceError -> Ghc (Either String String)
handleErrMsg opt err = do
    dflag <- G.getSessionDynFlags
    style <- getStyle
    let ret = convert opt . errBagToStrList dflag style . srcErrorMessages $ err
    return (Left ret)

errBagToStrList :: DynFlags -> PprStyle -> Bag ErrMsg -> [String]
errBagToStrList dflag style = map (ppErrMsg dflag style) . reverse . bagToList

----------------------------------------------------------------

ppErrMsg :: DynFlags -> PprStyle -> ErrMsg -> String
ppErrMsg dflag style err = ppMsg spn SevError dflag style msg ++ ext
   where
     spn = Gap.errorMsgSpan err
     msg = errMsgShortDoc err
     ext = showPage dflag style (errMsgExtraInfo err)

ppMsg :: SrcSpan -> Severity-> DynFlags -> PprStyle -> SDoc -> String
ppMsg spn sev dflag style msg = prefix ++ cts
  where
    cts  = showPage dflag style msg
    defaultPrefix
      | Gap.isDumpSplices dflag = ""
      | otherwise               = checkErrorPrefix
    prefix = fromMaybe defaultPrefix $ do
        (line,col,_,_) <- Gap.getSrcSpan spn
        file <- normalise <$> Gap.getSrcFile spn
        let severityCaption = Gap.showSeverityCaption sev
        return $ file ++ ":" ++ show line ++ ":" ++ show col ++ ":" ++ severityCaption

checkErrorPrefix :: String
checkErrorPrefix = "Dummy:0:0:Error:"
