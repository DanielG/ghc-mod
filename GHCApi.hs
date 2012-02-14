module GHCApi where

import Control.Monad
import CoreMonad
import DynFlags
import ErrMsg
import Exception
import GHC
import GHC.Paths (libdir)
import Types

----------------------------------------------------------------

withGHC :: (MonadPlus m) => Ghc (m a) -> IO (m a)
withGHC body = ghandle ignore $ runGhc (Just libdir) body
  where
    ignore :: (MonadPlus m) => SomeException -> IO (m a)
    ignore _ = return mzero

----------------------------------------------------------------

initSession0 :: Options -> Ghc [PackageId]
initSession0 opt = getSessionDynFlags >>=
  (>>= setSessionDynFlags) . setGhcFlags opt

initSession :: Options -> [String] -> [FilePath] -> Bool -> Ghc LogReader
initSession opt cmdOpts idirs logging = do
    dflags <- getSessionDynFlags
    let opts = map noLoc cmdOpts
    (dflags',_,_) <- parseDynamicFlags dflags opts
    (dflags'',readLog) <- liftIO . (>>= setLogger logging) . setGhcFlags opt . setFlags dflags' $ idirs
    setSessionDynFlags dflags''
    return readLog

----------------------------------------------------------------

setFlags :: DynFlags -> [FilePath] -> DynFlags
setFlags d idirs = d'
  where
    d' = d {
        packageFlags = ghcPackage : packageFlags d
      , importPaths = idirs
      , ghcLink = NoLink
      , hscTarget = HscInterpreted
      }

ghcPackage :: PackageFlag
ghcPackage = ExposePackage "ghc"

setGhcFlags :: Monad m => Options -> DynFlags -> m DynFlags
setGhcFlags opt flagset =
  do (flagset',_,_) <- parseDynamicFlags flagset (map noLoc (ghcOpts opt))
     return flagset'

----------------------------------------------------------------

setTargetFile :: (GhcMonad m) => String -> m ()
setTargetFile file = do
    target <- guessTarget file Nothing
    setTargets [target]
