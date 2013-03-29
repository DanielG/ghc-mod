{-# LANGUAGE CPP #-}

module Gap (
    Gap.ClsInst
  , mkTarget
  , withStyle
  , setLogAction
  , supportedExtensions
  , getSrcSpan
  , getSrcFile
  , setCtx
  , fOptions
  , toStringBuffer
  , liftIO
  , showSeverityCaption
#if __GLASGOW_HASKELL__ >= 702
#else
  , module Pretty
#endif
  ) where

import Control.Applicative hiding (empty)
import Control.Monad
import Data.Time.Clock
import DynFlags
import ErrUtils
import FastString
import GHC
import GHCChoice
import Outputable
import StringBuffer

import qualified InstEnv
import qualified Pretty
import qualified StringBuffer as SB


#if __GLASGOW_HASKELL__ >= 702
import CoreMonad (liftIO)
#else
import HscTypes (liftIO)
import Pretty
#endif

#if __GLASGOW_HASKELL__ < 706
import Control.Arrow
import Data.Convertible
#endif

{-
pretty :: Outputable a => a -> String
pretty = showSDocForUser neverQualify . ppr

debug :: Outputable a => a -> b -> b
debug x v = trace (pretty x) v
-}

----------------------------------------------------------------
----------------------------------------------------------------
--
#if __GLASGOW_HASKELL__ >= 706
type ClsInst = InstEnv.ClsInst
#else
type ClsInst = InstEnv.Instance
#endif

mkTarget :: TargetId -> Bool -> Maybe (SB.StringBuffer, UTCTime) -> Target
#if __GLASGOW_HASKELL__ >= 706
mkTarget = Target
#else
mkTarget tid allowObjCode = Target tid allowObjCode . (fmap . second) convert
#endif

----------------------------------------------------------------
----------------------------------------------------------------

withStyle :: DynFlags -> PprStyle -> SDoc -> Pretty.Doc
#if __GLASGOW_HASKELL__ >= 706
withStyle = withPprStyleDoc
#else
withStyle _ = withPprStyleDoc
#endif

setLogAction :: DynFlags
             -> (DynFlags -> Severity -> SrcSpan -> PprStyle -> SDoc -> IO ())
             -> DynFlags
setLogAction df f =
#if __GLASGOW_HASKELL__ >= 706
    df { log_action = f }
#else
    df { log_action = f df }
#endif

----------------------------------------------------------------
----------------------------------------------------------------

supportedExtensions :: [String]
#if __GLASGOW_HASKELL__ >= 700
supportedExtensions = supportedLanguagesAndExtensions
#else
supportedExtensions = supportedLanguages
#endif

----------------------------------------------------------------
----------------------------------------------------------------

getSrcSpan :: SrcSpan -> Maybe (Int,Int,Int,Int)
#if __GLASGOW_HASKELL__ >= 702
getSrcSpan (RealSrcSpan spn)
#else
getSrcSpan spn | isGoodSrcSpan spn
#endif
             = Just (srcSpanStartLine spn
                   , srcSpanStartCol spn
                   , srcSpanEndLine spn
                   , srcSpanEndCol spn)
getSrcSpan _ = Nothing

getSrcFile :: SrcSpan -> Maybe String
#if __GLASGOW_HASKELL__ >= 702
getSrcFile (RealSrcSpan spn)       = Just . unpackFS . srcSpanFile $ spn
#else
getSrcFile spn | isGoodSrcSpan spn = Just . unpackFS . srcSpanFile $ spn
#endif
getSrcFile _ = Nothing

----------------------------------------------------------------

toStringBuffer :: [String] -> Ghc StringBuffer
#if __GLASGOW_HASKELL__ >= 702
toStringBuffer = return . stringToStringBuffer . unlines
#else
toStringBuffer = liftIO . stringToStringBuffer . unlines
#endif

----------------------------------------------------------------

fOptions :: [String]
#if __GLASGOW_HASKELL__ >= 704
fOptions = [option | (option,_,_) <- fFlags]
        ++ [option | (option,_,_) <- fWarningFlags]
        ++ [option | (option,_,_) <- fLangFlags]
#elif __GLASGOW_HASKELL__ == 702
fOptions = [option | (option,_,_,_) <- fFlags]
#else
fOptions = [option | (option,_,_) <- fFlags]
#endif

----------------------------------------------------------------
----------------------------------------------------------------

setCtx :: [ModSummary] -> Ghc Bool
#if __GLASGOW_HASKELL__ >= 704
setCtx ms = do
#if __GLASGOW_HASKELL__ >= 706
    let modName = IIModule . moduleName . ms_mod
#else
    let modName = IIModule . ms_mod
#endif
    top <- map modName <$> filterM isTop ms
    setContext top
    return (not . null $ top)
#else
setCtx ms = do
    top <- map ms_mod <$> filterM isTop ms
    setContext top []
    return (not . null $ top)
#endif
  where
    isTop mos = lookupMod ||> returnFalse
      where
        lookupMod = lookupModule (ms_mod_name mos) Nothing >> return True
        returnFalse = return False


showSeverityCaption :: Severity -> String
#if __GLASGOW_HASKELL__ >= 706
showSeverityCaption SevWarning = "Warning: "
showSeverityCaption _          = ""
#else
showSeverityCaption = const ""
#endif
