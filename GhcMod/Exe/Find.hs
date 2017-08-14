{-# LANGUAGE CPP, BangPatterns, TupleSections, DeriveGeneric #-}

module GhcMod.Exe.Find
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
  ) where

import qualified GHC as G
import FastString
import Module
import OccName
import HscTypes
import Exception

import GhcMod.Convert
import GhcMod.Gap
import GhcMod.Monad
import GhcMod.Output
import GhcMod.Types
import GhcMod.Utils
import GhcMod.World
import GhcMod.LightGhc

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans.Control
import Control.Concurrent

import Data.List
import Data.Binary
import Data.Function
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef

import System.Directory.ModTime
import System.IO.Unsafe

import GHC.Generics (Generic)

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import GhcMod.PathsAndFiles
import System.Directory
import Prelude

----------------------------------------------------------------

-- | Type of function and operation names.
type Symbol = BS.ByteString
type ModuleNameBS = BS.ByteString

-- | Database from 'Symbol' to \['ModuleString'\].
data SymbolDb = SymbolDb
  { sdTable             :: Map Symbol (Set ModuleNameBS)
  , sdTimestamp         :: ModTime
  } deriving (Generic)

#if __GLASGOW_HASKELL__ >= 708
instance Binary SymbolDb
#else
instance Binary SymbolDb where
  put (SymbolDb a b) = put a >> put b
  get = do
    a <- get
    b <- get
    return (SymbolDb a b)
#endif
instance NFData SymbolDb

isOutdated :: IOish m => SymbolDb -> GhcModT m Bool
isOutdated db =
  isOlderThan (sdTimestamp db) <$> timedPackageCaches

----------------------------------------------------------------

-- | Looking up 'SymbolDb' with 'Symbol' to \['ModuleString'\]
--   which will be concatenated. 'loadSymbolDb' is called internally.
findSymbol :: IOish m => String -> GhcModT m String
findSymbol sym = loadSymbolDb' >>= lookupSymbol sym

-- | Looking up 'SymbolDb' with 'Symbol' to \['ModuleString'\]
--   which will be concatenated.
lookupSymbol :: IOish m => String -> SymbolDb -> GhcModT m String
lookupSymbol sym db = convert' $ lookupSym (fastStringToByteString $ mkFastString sym) db

lookupSym :: Symbol -> SymbolDb -> [ModuleString]
lookupSym sym db = map (ModuleString . unpackFS . mkFastStringByteString') $ S.toList $ M.findWithDefault S.empty sym $ sdTable db

---------------------------------------------------------------

loadSymbolDb' :: IOish m => GhcModT m SymbolDb
loadSymbolDb' = do
  cache <- symbolCache <$> cradle
  let doLoad True = do
        db <- decode <$> liftIO (LBS.readFile cache)
        outdated <- isOutdated db
        if outdated
        then doLoad False
        else return db
      doLoad False = do
        db <- loadSymbolDb
        liftIO $ LBS.writeFile cache $ encode db
        return db
  doLoad =<< liftIO (doesFileExist cache)

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
  st <- runGmPkgGhc $ getGlobalSymbolTable
  liftIO . LBS.putStr $ encode SymbolDb {
      sdTable = st
    , sdTimestamp = ts
    }

-- | Check whether given file is older than any file from the given set.
-- Returns True if given file does not exist.
isOlderThan :: ModTime -> [TimedFile] -> Bool
isOlderThan tCache files =
  any (tCache <=) $ map tfTime files -- including equal just in case

-- | Browsing all functions in all system modules.
getGlobalSymbolTable :: (G.GhcMonad m, MonadIO m)
                     => m (Map Symbol (Set ModuleNameBS))
getGlobalSymbolTable =
  foldM extend M.empty =<< (listVisibleModules <$> G.getSessionDynFlags)

extend :: (G.GhcMonad m, MonadIO m)
       => Map Symbol (Set ModuleNameBS)
       -> Module
       -> m (Map Symbol (Set ModuleNameBS))
extend mm mdl = do
  hsc_env <- G.getSession
  eps <- liftIO $ readIORef $ hsc_EPS hsc_env
  modinfo <- liftIO $ unsafeInterleaveIO $ runLightGhc hsc_env $ do
    G.getModuleInfo mdl <* liftIO (writeIORef (hsc_EPS hsc_env) eps)
  return $ M.unionWith S.union mm $ extractBindings modinfo mdl

extractBindings :: Maybe G.ModuleInfo
                -> G.Module
                -> Map Symbol (Set ModuleNameBS)
extractBindings Nothing  _   = M.empty
extractBindings (Just inf) mdl = M.fromList $ do
  name <- G.modInfoExports inf
  let sym = fastStringToByteString $ occNameFS $ G.getOccName name
      mdls = S.singleton $ fastStringToByteString $ moduleNameFS $ moduleName mdl
  return (sym, mdls)

mkFastStringByteString' :: BS.ByteString -> FastString
#if !MIN_VERSION_ghc(7,8,0)
fastStringToByteString :: FastString -> BS.ByteString
fastStringToByteString = BS.pack . bytesFS

mkFastStringByteString' = mkFastStringByteList . BS.unpack
#elif __GLASGOW_HASKELL__ == 708
mkFastStringByteString' = unsafePerformIO . mkFastStringByteString
#else
mkFastStringByteString' = mkFastStringByteString
#endif

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
