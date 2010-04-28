module Check (checkSyntax) where

import Bag
import Control.Applicative
import Control.Monad
import Data.IORef
import DynFlags
import ErrUtils
import Exception
import FastString
import GHC
import GHC.Paths (libdir)
import HscTypes
import Outputable hiding (showSDoc)
import Param
import Pretty
import System.Directory
import System.FilePath

----------------------------------------------------------------

checkSyntax :: Options -> String -> IO String
checkSyntax opt file = do
    let outdir = outDir opt
        objfile = objectFile outdir file
    makeDirectory outdir
    removeObjFile objfile
    unlines <$> check file outdir

----------------------------------------------------------------

cmdOptions :: [Located String]
cmdOptions = map noLoc ["-Wall","-fno-warn-unused-do-bind"]

check :: String -> String -> IO [String]
check fileName dir = ghandle ignore $ runGhc (Just libdir) $ do
    ref <- liftIO $ newIORef []
    initSession
    setTargetFile fileName
    loadWithLogger (refLogger ref) LoadAllTargets
    liftIO $ readIORef ref
  where
    initSession = do
        dflags <- getSessionDynFlags
        (dflags',_,_) <- parseDynamicFlags dflags cmdOptions
        setSessionDynFlags $ setImportPath $ setOutputDir dir dflags'
    setTargetFile file = do
        target <- guessTarget file Nothing
        setTargets [target]
    ignore :: SomeException -> IO [String]
    ignore _ = return []

----------------------------------------------------------------

refLogger :: IORef [String] -> WarnErrLogger
refLogger ref Nothing = do
    warns <- map showErrMsg . bagToList <$> getWarnings
    liftIO $ writeIORef ref warns
    clearWarnings
refLogger ref (Just e) = do
    let errs = map showErrMsg . bagToList . srcErrorMessages $ e
    liftIO $ writeIORef ref errs
    clearWarnings

----------------------------------------------------------------

setOutputDir :: String -> DynFlags -> DynFlags
setOutputDir f d = d {
    objectDir  = Just f
  , hiDir      = Just f
  , stubDir    = Just f, includePaths = f : includePaths d
  }

setImportPath :: DynFlags -> DynFlags
setImportPath d = d {
    importPaths = importPaths d ++ ["..","../..","../../..","../../../../.."]
  }

{-
setGhcPackage :: DynFlags -> DynFlags
setGhcPackage d = d {
    packageFlags = ExposePackage "ghc" : packageFlags d
  }
-}

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

----------------------------------------------------------------

makeDirectory :: FilePath -> IO ()
makeDirectory dir = makeDirectoryRecur $ normalise dir
  where
    makeDirectoryRecur "" = return ()
    makeDirectoryRecur cur = do
      exist <- doesDirectoryExist cur
      let par = takeDirectory cur
      unless exist $ do
          makeDirectoryRecur par
          createDirectory cur

objectFile :: FilePath -> FilePath -> FilePath
objectFile dir hsfile = dir </> replaceExtension hsfile ".o"

removeObjFile :: FilePath -> IO ()
removeObjFile objfile = do
    exist <- doesFileExist objfile
    when exist $ removeFile objfile
