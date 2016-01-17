{-# LANGUAGE CPP, DeriveGeneric #-}

module Language.Haskell.GhcMod.Find
#ifndef SPEC
  ( Symbol
  , SymbolDb
  , loadSymbolDb
  , lookupSymbol
  , dumpSymbol
  , findSymbol
  , lookupSym
  , isOutdated
  -- * Load 'SymbolDb' asynchronously
  , AsyncSymbolDb
  , newAsyncSymbolDb
  , getAsyncSymbolDb
  )
#endif
  where

import Language.Haskell.GhcMod.Convert
import Language.Haskell.GhcMod.Gap
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Output
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Utils
import Language.Haskell.GhcMod.World

import qualified GHC as G
import Name
import Module
import Exception

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Control
import Control.Concurrent
import Control.DeepSeq
import Data.Function
import Data.List
import qualified Data.ByteString.Lazy as BS
import Data.Binary
import GHC.Generics (Generic)
import Data.Map (Map)
import qualified Data.Map as M
import System.Directory.ModTime
import Prelude

----------------------------------------------------------------

-- | Type of function and operation names.
type Symbol = String
-- | Database from 'Symbol' to \['ModuleString'\].
data SymbolDb = SymbolDb
  { sdTable             :: Map Symbol [ModuleString]
  , sdTimestamp         :: ModTime
  } deriving (Generic)

instance Binary SymbolDb
instance NFData SymbolDb

isOutdated :: IOish m => SymbolDb -> GhcModT m Bool
isOutdated db =
  isOlderThan (sdTimestamp db) <$> timedPackageCaches

----------------------------------------------------------------

-- | Looking up 'SymbolDb' with 'Symbol' to \['ModuleString'\]
--   which will be concatenated. 'loadSymbolDb' is called internally.
findSymbol :: IOish m => Symbol -> GhcModT m String
findSymbol sym = loadSymbolDb >>= lookupSymbol sym

-- | Looking up 'SymbolDb' with 'Symbol' to \['ModuleString'\]
--   which will be concatenated.
lookupSymbol :: IOish m => Symbol -> SymbolDb -> GhcModT m String
lookupSymbol sym db = convert' $ lookupSym sym db

lookupSym :: Symbol -> SymbolDb -> [ModuleString]
lookupSym sym db = M.findWithDefault [] sym $ sdTable db

---------------------------------------------------------------

-- | Loading a file and creates 'SymbolDb'.
loadSymbolDb :: IOish m => GhcModT m SymbolDb
loadSymbolDb = do
  ghcMod <- liftIO ghcModExecutable
  readProc <- gmReadProcess'
  out <- liftIO $ readProc ghcMod ["--verbose", "error", "dumpsym"] ""
  return $!! decode out

----------------------------------------------------------------
-- used 'ghc-mod dumpsym'

-- | Dumps a 'Binary' representation of 'SymbolDb' to stdout
dumpSymbol :: IOish m => GhcModT m ()
dumpSymbol = do
  ts <- liftIO getCurrentModTime
  st <- runGmPkgGhc getGlobalSymbolTable
  liftIO . BS.putStr $ encode SymbolDb {
      sdTable = M.fromAscList st
    , sdTimestamp = ts
    }

-- | Check whether given file is older than any file from the given set.
-- Returns True if given file does not exist.
isOlderThan :: ModTime -> [TimedFile] -> Bool
isOlderThan tCache files =
  any (tCache <=) $ map tfTime files -- including equal just in case

-- | Browsing all functions in all system modules.
getGlobalSymbolTable :: LightGhc [(Symbol, [ModuleString])]
getGlobalSymbolTable = do
  df  <- G.getSessionDynFlags
  let mods = listVisibleModules df
  moduleInfos <- mapM G.getModuleInfo mods
  return $ collectModules
         $ extractBindings `concatMap` (moduleInfos `zip` mods)

extractBindings :: (Maybe G.ModuleInfo, G.Module)
                -> [(Symbol, ModuleString)]
extractBindings (Nothing,  _)   = []
extractBindings (Just inf, mdl) =
  map (\name -> (getOccString name, modStr)) names
  where
    names  = G.modInfoExports inf
    modStr = ModuleString $ moduleNameString $ moduleName mdl

collectModules :: [(Symbol, ModuleString)]
               -> [(Symbol, [ModuleString])]
collectModules = map tieup . groupBy ((==) `on` fst) . sort
  where
    tieup x = (head (map fst x), map snd x)

----------------------------------------------------------------

data AsyncSymbolDb = AsyncSymbolDb (MVar (Either SomeException SymbolDb))

asyncLoadSymbolDb :: IOish m
                  => MVar (Either SomeException SymbolDb)
                  -> GhcModT m ()
asyncLoadSymbolDb mv = void $
    liftBaseWith $ \run -> forkIO $ void $ run $ do
      edb <- gtry loadSymbolDb
      liftIO $ putMVar mv edb

newAsyncSymbolDb :: IOish m => GhcModT m AsyncSymbolDb
newAsyncSymbolDb = do
    mv <- liftIO newEmptyMVar
    asyncLoadSymbolDb mv
    return $ AsyncSymbolDb mv

getAsyncSymbolDb :: forall m. IOish m => AsyncSymbolDb -> GhcModT m SymbolDb
getAsyncSymbolDb (AsyncSymbolDb mv) = do
  db <- liftIO $ handleEx <$> takeMVar mv
  outdated <- isOutdated db
  if outdated
    then do
      asyncLoadSymbolDb mv
      liftIO $ handleEx <$> readMVar mv
    else do
      liftIO $ putMVar mv $ Right db
      return db
 where
   handleEx edb =
     case edb of
       Left ex -> throw ex
       Right db -> db
