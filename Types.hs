module Types where

import Control.Monad
import CoreMonad
import DynFlags
import ErrMsg
import Exception
import GHC
import GHC.Paths (libdir)

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

setTargetFile :: (GhcMonad m) => String -> m ()
setTargetFile file = do
    target <- guessTarget file Nothing
    setTargets [target]
