{-# OPTIONS_GHC -fno-warn-orphans #-}
module TestUtils (
    run
  , runD
  , runD'
  , runE
  , runNullLog
  , runGmOutDef
  , runLogDef
  , shouldReturnError
  , isPkgDbAt
  , isPkgConfDAt
  , bracketTagged
  , module Language.Haskell.GhcMod.Monad
  , module Language.Haskell.GhcMod.Types
  ) where

import Language.Haskell.GhcMod.Logging
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Cradle
import Language.Haskell.GhcMod.Types

import Control.Arrow
import Control.Category
import Control.Applicative
import Control.Monad
import Control.Monad.Error (ErrorT, runErrorT)
import Control.Monad.Trans.Journal
import Data.List.Split
import Data.Label
import Data.String
import Dir
import System.FilePath
import System.Directory
import System.Process
import Test.Hspec
import Prelude hiding ((.))

import Exception

testLogLevel :: GmLogLevel
testLogLevel = GmDebug

extract :: Show e => IO (Either e a, w) -> IO a
extract action = do
  (r,_) <- action
  case r of
    Right a ->  return a
    Left e -> error $ show e

runGhcModTSpec :: Options -> GhcModT IO a -> IO (Either GhcModError a, GhcModLog)
runGhcModTSpec opt action = do
  dir <- getCurrentDirectory
  runGhcModTSpec' dir opt action

runGhcModTSpec' :: IOish m
    => FilePath -> Options -> GhcModT m b -> m (Either GhcModError b, GhcModLog)
runGhcModTSpec' dir opt action = liftIO (canonicalizePath dir) >>= \dir' -> do
  runGmOutT opt $
    withGhcModEnv' withSpecCradle dir' opt $ \(env,_) -> do
      first (fst <$>) <$> runGhcModT' env defaultGhcModState
        (gmSetLogLevel (ooptLogLevel $ optOutput opt) >> action)
 where
   withSpecCradle :: (IOish m, GmOut m) => FilePath -> ((Cradle, GhcModLog) -> m a) -> m a
   withSpecCradle cradledir f =
     gbracket
       (runJournalT $ findSpecCradle (optPrograms opt) cradledir)
       (liftIO . cleanupCradle . fst) f


-- | Run GhcMod
run :: Options -> GhcModT IO a -> IO a
run opt a = extract $ runGhcModTSpec opt a

-- | Run GhcMod with default options
runD :: GhcModT IO a -> IO a
runD =
    extract . runGhcModTSpec (setLogLevel testLogLevel defaultOptions)

runD' :: FilePath -> GhcModT IO a -> IO a
runD' dir =
    extract . runGhcModTSpec' dir (setLogLevel testLogLevel defaultOptions)

setLogLevel :: GmLogLevel -> Options -> Options
setLogLevel = set (lOoptLogLevel . lOptOutput)

runE :: ErrorT e IO a -> IO (Either e a)
runE = runErrorT

runNullLog :: MonadIO m => JournalT GhcModLog m a -> m a
runNullLog action = do
  (a,w) <- runJournalT action
  liftIO $ print w
  return a

runGmOutDef :: IOish m => GmOutT m a -> m a
runGmOutDef = runGmOutT defaultOptions

runLogDef :: IOish m => GmOutT (JournalT GhcModLog m) a -> m a
runLogDef = fmap fst . runJournalT . runGmOutDef

shouldReturnError :: Show a
                  => IO (Either GhcModError a, GhcModLog)
                  -> Expectation
shouldReturnError action = do
  (a,_) <- action
  a `shouldSatisfy` isLeft
 where
   isLeft (Left _) = True
   isLeft _ = False

isPkgConfD :: FilePath -> Bool
isPkgConfD d = let
    (_dir, pkgconfd) = splitFileName d
    in case splitOn "-" pkgconfd of
         [_arch, _platform, _compiler, _compver, "packages.conf.d"] -> True
         _ -> False

isPkgConfDAt :: FilePath -> FilePath -> Bool
isPkgConfDAt d d' | d == takeDirectory d' && isPkgConfD d' = True
isPkgConfDAt _ _ = False

isPkgDbAt :: FilePath -> GhcPkgDb -> Bool
isPkgDbAt d (PackageDb dir) = isPkgConfDAt d dir
isPkgDbAt _ _ = False

instance IsString ModuleName where
    fromString = mkModuleName

-- | Bracketing function for test/data/options-cradle environment to install tagged in package-db-a
bracketTagged :: IO a -> IO a
bracketTagged m = do
    withDirectory_ "test/data/options-cradle" $ do
        void $ system "cabal install --global --package-db=package-db-a --prefix=$(pwd) tagged"
        putStrLn "start"
        void $ system "ghc-pkg list --global --user --package-db=package-db-a tagged"
        putStrLn "stop"
        res <- m
        void $ system "ghc-pkg --package-db=package-db-a unregister tagged"
        -- void $ system "cabal sandbox delete"
        return res
