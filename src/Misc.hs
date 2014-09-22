{-# LANGUAGE DeriveDataTypeable #-}

module Misc (
    GHCModiError(..)
  , Restart(..)
  , World
  , getCurrentWorld
  , isWorldChanged
  , UnGetLine
  , emptyNewUnGetLine
  , ungetCommand
  , getCommand
  , SymDbReq
  , newSymDbReq
  , getDb
  , checkDb
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent.Async (Async, async, wait)
import Control.Exception (Exception)
import CoreMonad (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Internal
import System.Directory (getModificationTime)

----------------------------------------------------------------

data GHCModiError = CmdArg [String] deriving (Show, Typeable)

instance Exception GHCModiError

----------------------------------------------------------------

data Restart = Restart deriving (Show, Typeable)

instance Exception Restart

----------------------------------------------------------------

data World = World {
    worldCabalFileModificationTime :: Maybe UTCTime
  } deriving (Show, Eq)

getCurrentWorld :: IOish m => GhcModT m World
getCurrentWorld = do
    crdl <- cradle
    mmt <- case cradleCabalFile crdl of
        Just file -> liftIO $ Just <$> getModificationTime file
        Nothing   -> return Nothing
    return $ World { worldCabalFileModificationTime = mmt }

isWorldChanged :: IOish m => World -> GhcModT m Bool
isWorldChanged world = do
    world' <- getCurrentWorld
    return (world /= world')

----------------------------------------------------------------

newtype UnGetLine = UnGetLine (IORef (Maybe String))

emptyNewUnGetLine :: IO UnGetLine
emptyNewUnGetLine = UnGetLine <$> newIORef Nothing

ungetCommand :: UnGetLine -> String -> IO ()
ungetCommand (UnGetLine ref) cmd = writeIORef ref (Just cmd)

getCommand :: UnGetLine -> IO String
getCommand (UnGetLine ref) = do
    mcmd <- readIORef ref
    case mcmd of
        Nothing -> getLine
        Just cmd -> do
            writeIORef ref Nothing
            return cmd

----------------------------------------------------------------

type SymDbReqAction = (Either GhcModError SymbolDb, GhcModLog)
data SymDbReq = SymDbReq (IORef (Async SymDbReqAction)) (IO SymDbReqAction)

newSymDbReq :: Options -> IO SymDbReq
newSymDbReq opt = do
    let act = runGhcModT opt loadSymbolDb
    req <- async act
    ref <- newIORef req
    return $ SymDbReq ref act

getDb :: IOish m => SymDbReq -> GhcModT m SymbolDb
getDb (SymDbReq ref _) = do
    req <- liftIO $ readIORef ref
    -- 'wait' really waits for the asynchronous action at the fist time.
    -- Then it reads a cached value from the second time.
    hoistGhcModT =<< liftIO (wait req)

checkDb :: IOish m => SymDbReq -> SymbolDb -> GhcModT m SymbolDb
checkDb (SymDbReq ref act) db = do
    outdated <- liftIO $ isOutdated db
    if outdated then do
        -- async and wait here is unnecessary because this is essentially
        -- synchronous. But Async can be used a cache.
        req <- liftIO $ async act
        liftIO $ writeIORef ref req
        hoistGhcModT =<< liftIO (wait req)
      else
        return db
