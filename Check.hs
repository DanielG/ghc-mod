module Check (checkSyntax) where

import Bag
import Cabal
import Control.Applicative
import Data.IORef
import ErrUtils
import Exception
import FastString
import GHC
import HscTypes
import Outputable hiding (showSDoc)
import Prelude hiding (catch)
import Pretty
import Types

----------------------------------------------------------------

checkSyntax :: Options -> String -> IO String
checkSyntax _ file = unlines <$> check file

----------------------------------------------------------------

check :: String -> IO [String]
check fileName = withGHC $ do
    ref <- newRef []
    (owdir,mdirfile) <- getDirs
    case mdirfile of
        Nothing -> do
            initSession options Nothing
            setTargetFile fileName
        Just (cdir,cfile) -> do
            midirs <- parseCabalFile cfile
            changeToCabalDirectory cdir
            let idirs = case midirs of
                    Nothing   -> [cdir,owdir]
                    Just dirs -> dirs ++ [owdir]
            initSession options (Just idirs)
            setTargetFile (ajustFileName fileName owdir cdir)
    loadWithLogger (refLogger ref) LoadAllTargets `gcatch` handleParseError ref
    clearWarnings
    readRef ref
  where
    options = ["-Wall","-fno-warn-unused-do-bind"]
    handleParseError ref e = do
        liftIO . writeIORef ref $ errBagToStrList . srcErrorMessages $ e
        return Succeeded
    newRef  = liftIO . newIORef
    readRef = liftIO . readIORef

----------------------------------------------------------------

refLogger :: IORef [String] -> WarnErrLogger
refLogger ref Nothing =
    (errBagToStrList <$> getWarnings) >>= liftIO . writeIORef ref
refLogger ref (Just e) =
    liftIO . writeIORef ref $ errBagToStrList . srcErrorMessages $ e

errBagToStrList :: Bag ErrMsg -> [String]
errBagToStrList = map showErrMsg . reverse . bagToList

----------------------------------------------------------------

showErrMsg :: ErrMsg -> String
showErrMsg err = file ++ ":" ++ line ++ ":" ++ col ++ ":" ++ msg ++ "\0" ++ ext
   where
     spn = head (errMsgSpans err)
     file = unpackFS (srcSpanFile spn)
     line = show (srcSpanStartLine spn)
     col  = show (srcSpanStartCol spn)
     msg = showSDoc (errMsgShortDoc err)
     ext = showSDoc (errMsgExtraInfo err)

style :: PprStyle
style = mkUserStyle neverQualify AllTheWay

showSDoc :: SDoc -> String
--showSDoc d = map toNull . Pretty.showDocWith ZigZagMode $ d style
showSDoc d = map toNull . Pretty.showDocWith PageMode $ d style
  where
    toNull '\n' = '\0'
    toNull x = x