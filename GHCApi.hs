module GHCApi where

import Cabal
import Control.Applicative
import Control.Exception
import CoreMonad
import Data.Maybe (isJust)
import DynFlags
import ErrMsg
import Exception
import GHC
import GHC.Paths (libdir)
import HeaderInfo
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

----------------------------------------------------------------

importDirs :: [IncludeDir]
importDirs = [".","..","../..","../../..","../../../..","../../../../.."]

initializeGHC :: Options -> Cradle -> FilePath -> [GHCOption] -> Bool -> Ghc LogReader
initializeGHC opt cradle fileName ghcOptions logging
  | cabal     =
      initSession opt ghcOptions importDirs Nothing Nothing logging fileName
  | otherwise = do
      (gopts,idirs,depPkgs,hdrExts) <- liftIO $ fromCabal ghcOptions cradle
      initSession opt gopts idirs (Just depPkgs) (Just hdrExts) logging fileName
  where
    cabal = isJust $ cradleCabalFile cradle

initSession :: Options
            -> [GHCOption]
            -> [IncludeDir]
            -> Maybe [Package]
            -> Maybe [LangExt]
            -> Bool
            -> FilePath
            -> Ghc LogReader
initSession opt cmdOpts idirs mDepPkgs mLangExts logging file = do
    dflags <- getSessionDynFlags
    hdrExts <- liftIO $ map unLoc <$> getOptionsFromFile dflags file
    let th = useTemplateHaskell mLangExts hdrExts
        opts = map noLoc cmdOpts
    (dflags',_,_) <- parseDynamicFlags dflags opts
    (dflags'',readLog) <- liftIO . (>>= setLogger logging)
                          . setGhcFlags opt . setFlags opt dflags' idirs mDepPkgs $ th
    _ <- setSessionDynFlags dflags''
    return readLog

useTemplateHaskell :: Maybe [LangExt] -> [HeaderExt] -> Bool
useTemplateHaskell mLangExts hdrExts = th1 || th2
  where
    th1 = "-XTemplateHaskell" `elem` hdrExts
    th2 = maybe False ("TemplateHaskell" `elem`) mLangExts

----------------------------------------------------------------

setFlags :: Options -> DynFlags -> [IncludeDir] -> Maybe [Package] -> Bool -> DynFlags
setFlags opt d idirs mDepPkgs th
  | expandSplice opt = dopt_set d' Opt_D_dump_splices
  | otherwise        = d'
  where
    d' = addDevPkgs mDepPkgs $ d {
        importPaths = idirs
      , ghcLink     = if th then LinkInMemory else NoLink
      , hscTarget   = if th then HscInterpreted else HscNothing
      , flags       = flags d
      }

addDevPkgs :: Maybe [Package] -> DynFlags -> DynFlags
addDevPkgs Nothing df     = df
addDevPkgs (Just pkgs) df = df' {
    packageFlags = map ExposePackage pkgs ++ packageFlags df
  }
  where
    df' = dopt_set df Opt_HideAllPackages

setGhcFlags :: Monad m => Options -> DynFlags -> m DynFlags
setGhcFlags opt flagset =
  do (flagset',_,_) <- parseDynamicFlags flagset (map noLoc (ghcOpts opt))
     return flagset'

----------------------------------------------------------------

setTargetFile :: (GhcMonad m) => String -> m ()
setTargetFile file = do
    target <- guessTarget file Nothing
    setTargets [target]
