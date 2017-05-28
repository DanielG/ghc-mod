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
  , module GhcMod.Monad
  , module GhcMod.Types
  ) where

import GhcMod.Logging
import GhcMod.Monad
import GhcMod.Cradle
import GhcMod.Types

import Control.Arrow
import Control.Category
import Control.Applicative
import Control.Monad.Error (ErrorT, runErrorT)
import Control.Monad.Trans.Journal
import Data.List.Split
import Data.Label
import Data.String
import System.FilePath
import System.Directory
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
