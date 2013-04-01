module GHCApi (
    withGHC
  , withGHCDummyFile
  , initializeFlags
  , initializeFlagsWithCradle
  , setTargetFile
  , getDynamicFlags
  , setSlowDynFlags
  , checkSlowAndSet
  , canCheckFast
  ) where

import CabalApi
import Control.Applicative
import Control.Exception
import Control.Monad
import CoreMonad
import Data.Maybe (isJust)
import DynFlags
import ErrMsg
import Exception
import GHC
import GHC.Paths (libdir)
import System.Exit
import System.IO
import Types

----------------------------------------------------------------

withGHCDummyFile :: Alternative m => Ghc (m a) -> IO (m a)
withGHCDummyFile = withGHC "Dummy"

withGHC :: Alternative m => FilePath -> Ghc (m a) -> IO (m a)
withGHC file body = ghandle ignore $ runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    defaultCleanupHandler dflags body
  where
    ignore :: Alternative m => SomeException -> IO (m a)
    ignore e = do
        hPutStr stderr $ file ++ ":0:0:Error:"
        hPrint stderr e
        exitSuccess

----------------------------------------------------------------

importDirs :: [IncludeDir]
importDirs = [".","..","../..","../../..","../../../..","../../../../.."]

initializeFlagsWithCradle :: Options -> Cradle -> [GHCOption] -> Bool -> Ghc LogReader
initializeFlagsWithCradle opt cradle ghcOptions logging
  | cabal     = do
      (gopts,idirs,depPkgs) <- liftIO $ fromCabalFile ghcOptions cradle
      initSession opt gopts idirs (Just depPkgs) logging
  | otherwise =
      initSession opt ghcOptions importDirs Nothing logging
  where
    cabal = isJust $ cradleCabalFile cradle

----------------------------------------------------------------

initSession :: Options
            -> [GHCOption]
            -> [IncludeDir]
            -> Maybe [Package]
            -> Bool
            -> Ghc LogReader
initSession opt cmdOpts idirs mDepPkgs logging = do
    dflags0 <- getSessionDynFlags
    (dflags1,readLog) <- setupDynamicFlags dflags0
    _ <- setSessionDynFlags dflags1
    return readLog
  where
    setupDynamicFlags df0 = do
        df1 <- modifyFlagsWithOpts df0 cmdOpts
        let df2 = modifyFlags df1 idirs mDepPkgs (expandSplice opt)
        df3 <- modifyFlagsWithOpts df2 $ ghcOpts opt
        liftIO $ setLogger logging df3

----------------------------------------------------------------

initializeFlags :: Options -> Ghc ()
initializeFlags opt = do
    dflags0 <- getSessionDynFlags
    dflags1 <- modifyFlagsWithOpts dflags0 $ ghcOpts opt
    void $ setSessionDynFlags dflags1

----------------------------------------------------------------

-- FIXME removing Options
modifyFlags :: DynFlags -> [IncludeDir] -> Maybe [Package] -> Bool -> DynFlags
modifyFlags d0 idirs mDepPkgs splice
  | splice    = setSplice d3
  | otherwise = d3
  where
    d1 = d0 { importPaths = idirs }
    d2 = setFastOrNot d1 Fast
    d3 = maybe d2 (addDevPkgs d2) mDepPkgs

setSplice :: DynFlags -> DynFlags
setSplice dflag = dopt_set dflag Opt_D_dump_splices

addDevPkgs :: DynFlags -> [Package] -> DynFlags
addDevPkgs df pkgs = df''
  where
    df' = dopt_set df Opt_HideAllPackages
    df'' = df' {
        packageFlags = map ExposePackage pkgs ++ packageFlags df
      }

----------------------------------------------------------------

setFastOrNot :: DynFlags -> CheckSpeed -> DynFlags
setFastOrNot dflags Slow = dflags {
    ghcLink   = LinkInMemory
  , hscTarget = HscInterpreted
  }
setFastOrNot dflags Fast = dflags {
    ghcLink   = NoLink
  , hscTarget = HscNothing
  }

setSlowDynFlags :: Ghc ()
setSlowDynFlags = (flip setFastOrNot Slow <$> getSessionDynFlags)
                  >>= void . setSessionDynFlags

-- To check TH, a session module graph is necessary.
-- "load" sets a session module graph using "depanal".
-- But we have to set "-fno-code" to DynFlags before "load".
-- So, this is necessary redundancy.
checkSlowAndSet :: Ghc ()
checkSlowAndSet = do
    fast <- canCheckFast <$> depanal [] False
    unless fast setSlowDynFlags

----------------------------------------------------------------

modifyFlagsWithOpts :: DynFlags -> [String] -> Ghc DynFlags
modifyFlagsWithOpts dflags cmdOpts =
    tfst <$> parseDynamicFlags dflags (map noLoc cmdOpts)
  where
    tfst (a,_,_) = a

----------------------------------------------------------------

setTargetFile :: (GhcMonad m) => String -> m ()
setTargetFile file = do
    target <- guessTarget file Nothing
    setTargets [target]

----------------------------------------------------------------

getDynamicFlags :: IO DynFlags
getDynamicFlags = runGhc (Just libdir) getSessionDynFlags

canCheckFast :: ModuleGraph -> Bool
canCheckFast = not . any (hasTHorQQ . ms_hspp_opts)
  where
    hasTHorQQ :: DynFlags -> Bool
    hasTHorQQ dflags = any (\opt -> xopt opt dflags) [Opt_TemplateHaskell, Opt_QuasiQuotes]
