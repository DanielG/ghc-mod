module Check (checkSyntax) where

import Bag
import Control.Applicative
import Data.IORef
import DynFlags
import ErrUtils
import FastString
import GHC
import HscTypes
import Outputable hiding (showSDoc)
import Pretty
import Types

----------------------------------------------------------------

checkSyntax :: Options -> String -> IO String
checkSyntax _ file = unlines <$> check file

----------------------------------------------------------------

check :: String -> IO [String]
check fileName = withGHC $ do
    ref <- liftIO $ newIORef []
    initSession
    setTargetFile fileName
    loadWithLogger (refLogger ref) LoadAllTargets
    liftIO $ readIORef ref
  where
    -- I don't know why, but parseDynamicFlags must be used.
    initSession = do
        dflags <- getSessionDynFlags
        (dflags',_,_) <- parseDynamicFlags dflags cmdOptions
        setSessionDynFlags $ setFlags dflags'
    setTargetFile file = do
        target <- guessTarget file Nothing
        setTargets [target]

-- I don't know why, but parseDynamicFlags must be used.
cmdOptions :: [Located String]
cmdOptions = map noLoc ["-Wall","-fno-warn-unused-do-bind"]

----------------------------------------------------------------

refLogger :: IORef [String] -> WarnErrLogger
refLogger ref Nothing = do
    warns <- map showErrMsg . reverse . bagToList <$> getWarnings
    liftIO $ writeIORef ref warns
    clearWarnings
refLogger ref (Just e) = do
    let errs = map showErrMsg . reverse . bagToList . srcErrorMessages $ e
    liftIO $ writeIORef ref errs
    clearWarnings

----------------------------------------------------------------

setFlags :: DynFlags -> DynFlags
setFlags d = d {
    importPaths = importPaths d ++ importDirs
  , packageFlags = ghcPackage : packageFlags d
  , ghcLink = NoLink
-- GHC.desugarModule does not produces the pattern warnings, why?
--  , hscTarget = HscNothing
  , hscTarget = HscInterpreted
  }

importDirs :: [String]
importDirs = ["..","../..","../../..","../../../../.."]

ghcPackage :: PackageFlag
ghcPackage = ExposePackage "ghc"

----------------------------------------------------------------

showErrMsg :: ErrMsg -> String
showErrMsg err = file ++ ":" ++ line ++ ":" ++ col ++ ":" ++ msg
   where
     spn = head (errMsgSpans err)
     file = unpackFS (srcSpanFile spn)
     line = show (srcSpanStartLine spn)
     col  = show (srcSpanStartCol spn)
     msg = showSDoc (errMsgShortDoc err)

style :: PprStyle
style = mkUserStyle neverQualify AllTheWay

showSDoc :: SDoc -> String
showSDoc d = Pretty.showDocWith OneLineMode (d style)
