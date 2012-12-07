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
withGHC = withGHC' "Dummy"

withGHC' :: Alternative m => FilePath -> Ghc (m a) -> IO (m a)
withGHC' file body = ghandle ignore $ runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    defaultCleanupHandler dflags body
  where
    ignore :: Alternative m => SomeException -> IO (m a)
    ignore e = do
        hPutStr stderr $ file ++ ":0:0:Error:"
        hPrint stderr e
        exitSuccess

----------------------------------------------------------------

initSession0 :: Options -> Ghc [PackageId]
initSession0 opt = getSessionDynFlags >>=
  (>>= setSessionDynFlags) . setGhcFlags opt

initSession :: Options -> [String] -> [FilePath] -> Maybe [String] -> Bool -> Ghc LogReader
initSession opt cmdOpts idirs mayPkgs logging = do
    dflags <- getSessionDynFlags
    let opts = map noLoc cmdOpts
    (dflags',_,_) <- parseDynamicFlags dflags opts
    (dflags'',readLog) <- liftIO . (>>= setLogger logging)
                          . setGhcFlags opt . setFlags opt dflags' idirs $ mayPkgs
    _ <- setSessionDynFlags dflags''
    return readLog

----------------------------------------------------------------

setFlags :: Options -> DynFlags -> [FilePath] -> Maybe [String] -> DynFlags
setFlags opt d idirs mayPkgs
  | expandSplice opt = dopt_set d' Opt_D_dump_splices
  | otherwise        = d'
  where
    d' = maySetExpose $ d {
        importPaths = idirs
      , ghcLink = LinkInMemory
      , hscTarget = HscInterpreted
      , flags = flags d
      }
    -- Do hide-all only when depend packages specified
    maySetExpose df = maybe df (\x -> (dopt_set df Opt_HideAllPackages) {
                                   packageFlags = map ExposePackage x ++ packageFlags df
                                   }) mayPkgs

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
