{-# LANGUAGE DeriveDataTypeable, CPP #-}

module Misc (
    SymDbReq
  , newSymDbReq
  , getDb
  , checkDb
  ) where

import Control.Concurrent.Async (Async, async, wait)
import CoreMonad (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Prelude

import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Internal hiding (MonadIO,liftIO)

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
    outdated <- isOutdated db
    if outdated then do
        -- async and wait here is unnecessary because this is essentially
        -- synchronous. But Async can be used a cache.
        req <- liftIO $ async act
        liftIO $ writeIORef ref req
        hoistGhcModT =<< liftIO (wait req)
      else
        return db
