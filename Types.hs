{-# LANGUAGE CPP #-}

module Types where

import Control.Applicative
import Control.Monad
import CoreMonad
import Data.IORef
import DynFlags
import ErrUtils
import Exception
import FastString
import GHC
import GHC.Paths (libdir)
import Outputable
import System.FilePath
import Pretty

----------------------------------------------------------------

data Options = Options {
    convert   :: [String] -> String
  , hlintOpts :: [String]
  , checkIncludes :: [String]
  , operators :: Bool
  , packageConfs :: [FilePath]
  , useUserPackageConf :: Bool
  }

withGHC :: (MonadPlus m) => Ghc (m a) -> IO (m a)
withGHC body = ghandle ignore $ runGhc (Just libdir) body
  where
    ignore :: (MonadPlus m) => SomeException -> IO (m a)
    ignore _ = return mzero

----------------------------------------------------------------

initSession0 :: Options -> Ghc [PackageId]
initSession0 opt = getSessionDynFlags >>=
  setSessionDynFlags . setPackageConfFlags opt

initSession :: Options -> [String] -> Maybe [FilePath] -> Bool -> Ghc LogReader
initSession opt cmdOpts midirs logging = do
    dflags <- getSessionDynFlags
    let opts = map noLoc cmdOpts
    (dflags',_,_) <- parseDynamicFlags dflags opts
    (dflags'',readLog) <- liftIO . setLogger logging . setPackageConfFlags opt . setFlags dflags' $ midirs
    setSessionDynFlags dflags''
    return readLog

----------------------------------------------------------------

setFlags :: DynFlags -> Maybe [FilePath] -> DynFlags
setFlags d midirs = maybe d' (\x -> d' { importPaths = x }) midirs
  where
    d' = d {
        packageFlags = ghcPackage : packageFlags d
      , ghcLink = NoLink
      , hscTarget = HscInterpreted
      }

ghcPackage :: PackageFlag
ghcPackage = ExposePackage "ghc"

setPackageConfFlags :: Options -> DynFlags -> DynFlags
setPackageConfFlags
  Options { packageConfs = confs, useUserPackageConf = useUser }
  flagset@DynFlags { extraPkgConfs = extra, flags = origFlags }
  = flagset { extraPkgConfs = extra', flags = flags' }
  where
    extra' = confs ++ extra
    flags' = if useUser then
                 origFlags
             else
                 filter (/=Opt_ReadUserPackageConf) origFlags

----------------------------------------------------------------

type LogReader = IO [String]

setLogger :: Bool -> DynFlags -> IO (DynFlags, LogReader)
setLogger False df = return (newdf, undefined)
  where
    newdf = df { log_action = \_ _ _ _ -> return () }
setLogger True  df = do
    ref <- newIORef [] :: IO (IORef [String])
    let newdf = df { log_action = appendLog ref }
    return (newdf, reverse <$> readIORef ref)
  where
    appendLog ref _ src _ msg = modifyIORef ref (\ls -> ppMsg src msg : ls)

ppMsg :: SrcSpan -> Message -> String
#if __GLASGOW_HASKELL__ >= 702
ppMsg (UnhelpfulSpan _) _ = undefined
ppMsg (RealSrcSpan src) msg
#else
ppMsg src msg
#endif
    = file ++ ":" ++ line ++ ":" ++ col ++ ":" ++ cts ++ "\0" -- xxx
  where
    file = takeFileName $ unpackFS (srcSpanFile src)
    line = show (srcSpanStartLine src)
    col  = show (srcSpanStartCol src)
    cts  = showMsg msg

style :: PprStyle
style = mkUserStyle neverQualify AllTheWay

showMsg :: SDoc -> String
showMsg d = map toNull . Pretty.showDocWith PageMode $ d style
  where
    toNull '\n' = '\0'
    toNull x = x

----------------------------------------------------------------

setTargetFile :: (GhcMonad m) => String -> m ()
setTargetFile file = do
    target <- guessTarget file Nothing
    setTargets [target]
