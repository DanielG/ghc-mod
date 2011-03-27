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
import System.FilePath
import System.Directory
import Text.Regex.Posix
import Types

----------------------------------------------------------------

checkSyntax :: Options -> String -> IO String
checkSyntax opt file = unlines <$> check opt file

----------------------------------------------------------------

check :: Options -> String -> IO [String]
check opt fileName = withGHC $ do
    file <- initializeGHC opt fileName options
    setTargetFile file
    ref <- newRef []
    initSession =<< liftIO parseGhciContents
    setTargetFile fileName
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
showErrMsg err = 
   if isGoodSrcSpan spn
     then file ++ ":" ++ line ++ ":" ++ col ++ ":" ++ msg ++ "\0" ++ ext
     else "bad span"
   where
     spn = head (errMsgSpans err)
     file = takeFileName $ unpackFS (srcSpanFile spn)
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

----------------------------------------------------------------

-- | Search this directory and all higher directories for .ghci files.
-- The search ends when either (1) a .ghci file is found, (2) the
-- user's home directory is encountered, or (3) the system root
-- directory is encountered.  This function returns the contents of
-- the .ghci file, or if no .ghci file is found, it returns a default
-- string of settings.
getGhciContents :: IO String
getGhciContents = do
  path <- getCurrentDirectory
  exists <- doesFileExist (path ++ "/.ghci")
  if exists
    then readFile (path ++ "/.ghci")
    else do
      home <- getHomeDirectory
      if path == home || path == "/"
        then return defaultFlags
        else do
          setCurrentDirectory ".."
          getGhciContents

parseGhciContents :: IO [String]
parseGhciContents = do
  cwd <- getCurrentDirectory
  ts <- getGhciContents
  setCurrentDirectory cwd
  let ls = filter (=~ "^:set ") (lines ts)
  if length ls == 0
    then return (findFlags [defaultFlags])
    else return (findFlags ls)
  where
    findFlags = concat . map (filter (=~ "^-[fwW]") . words) 

defaultFlags :: String
defaultFlags = ":set -Wall -fno-warn-unused-do-bind"
