module GHCApi where

import Control.Applicative
import Control.Exception
import CoreMonad
import DynFlags
import ErrMsg
import Exception
import GHC
import GHC.Paths (libdir)
import System.Exit
import System.IO
import Types

----------------------------------------------------------------

withGHC :: Alternative m => Ghc (m a) -> IO (m a)
withGHC body = ghandle ignore $ runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    defaultCleanupHandler dflags body
  where
    ignore :: Alternative m => SomeException -> IO (m a)
    ignore e = do
        hPutStr stderr "Dummy:0:0:Error:"
        hPrint stderr e
        exitSuccess

----------------------------------------------------------------

initSession0 :: Options -> Ghc [PackageId]
initSession0 opt = getSessionDynFlags >>=
  (>>= setSessionDynFlags) . setGhcFlags opt

initSession :: Options -> [String] -> [FilePath] -> Bool -> Ghc LogReader
initSession opt cmdOpts idirs logging = do
    dflags <- getSessionDynFlags
    let opts = map noLoc cmdOpts
    (dflags',_,_) <- parseDynamicFlags dflags opts
    (dflags'',readLog) <- liftIO . (>>= setLogger logging) . setGhcFlags opt . setFlags opt dflags' $ idirs
    setSessionDynFlags dflags''
    return readLog

----------------------------------------------------------------

setFlags :: Options -> DynFlags -> [FilePath] -> DynFlags
setFlags opt d idirs
  | expandSplice opt = dopt_set d' Opt_D_dump_splices
  | otherwise        = d'
  where
    d' = d {
        packageFlags = ghcPackage : packageFlags d
      , importPaths = idirs
      , ghcLink = LinkInMemory
      , hscTarget = HscInterpreted
      , flags = flags d
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
