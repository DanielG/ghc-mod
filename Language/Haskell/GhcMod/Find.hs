{-# LANGUAGE CPP, BangPatterns, DoAndIfThenElse, TupleSections #-}

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

import Control.Applicative
import Control.Monad
import Control.Exception
import Control.Concurrent
import Data.List
import Data.Binary
import Data.IORef
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified GHC as G
import FastString
import Module
import OccName
import HscTypes
import Language.Haskell.GhcMod.Convert
import Language.Haskell.GhcMod.Gap
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Output
import Language.Haskell.GhcMod.PathsAndFiles
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Utils
import Language.Haskell.GhcMod.World
import Language.Haskell.GhcMod.Target
import Language.Haskell.GhcMod.LightGhc

import Exception

import Control.Monad.Trans.Control
import Data.Function
import System.Directory
import System.Directory.ModTime
import System.FilePath ((</>))
import System.IO
import System.IO.Unsafe
import Prelude

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

----------------------------------------------------------------

-- | Type of function and operation names.
type Symbol = BS.ByteString
type ModuleNameBS = BS.ByteString

-- | Database from 'Symbol' to \['ModuleString'\].
data SymbolDb = SymbolDb
  { table             :: Map Symbol [ModuleNameBS]
  , symbolDbCachePath :: FilePath
  }

isOutdated :: IOish m => SymbolDb -> GhcModT m Bool
isOutdated db =
  (liftIO . isOlderThan (symbolDbCachePath db)) =<< timedPackageCaches

----------------------------------------------------------------

-- | Looking up 'SymbolDb' with 'Symbol' to \['ModuleString'\]
--   which will be concatenated. 'loadSymbolDb' is called internally.
findSymbol :: IOish m => String -> GhcModT m String
findSymbol sym = do
  tmpdir <- cradleTempDir <$> cradle
  loadSymbolDb tmpdir >>= lookupSymbol sym

-- | Looking up 'SymbolDb' with 'Symbol' to \['ModuleString'\]
--   which will be concatenated.
lookupSymbol :: IOish m => String -> SymbolDb -> GhcModT m String
lookupSymbol sym db = convert' $ lookupSym (fastStringToByteString $ mkFastString sym) db

lookupSym :: Symbol -> SymbolDb -> [ModuleString]
lookupSym sym db = map (ModuleString . unpackFS . mkFastStringByteString') $ M.findWithDefault [] sym $ table db

---------------------------------------------------------------

-- | Loading a file and creates 'SymbolDb'.
loadSymbolDb :: IOish m => FilePath -> GhcModT m SymbolDb
loadSymbolDb dir = do
  ghcMod <- liftIO ghcModExecutable
  readProc <- gmReadProcess
  file   <- liftIO $ chop <$> readProc ghcMod ["dumpsym", dir] ""
  !db    <- M.fromList . decode <$> liftIO (LBS.readFile file)
  return $ SymbolDb
    { table             = db
    , symbolDbCachePath = file
    }
  where
    chop :: String -> String
    chop "" = ""
    chop xs = init xs

----------------------------------------------------------------
-- used 'ghc-mod dumpsym'

-- | Dumping a set of ('Symbol',\['ModuleString'\]) to a file
--   if the file does not exist or is invalid.
--   The file name is printed.

dumpSymbol :: IOish m => FilePath -> GhcModT m String
dumpSymbol dir = do
  create <- (liftIO . isOlderThan cache) =<< timedPackageCaches
  pkgOpts <- packageGhcOptions
  when create $ liftIO $ do
    withLightHscEnv pkgOpts $ \env -> do
      writeSymbolCache cache =<< getGlobalSymbolTable env

  return $ unlines [cache]
  where
    cache = dir </> symbolCacheFile

writeSymbolCache :: FilePath
                 -> Map Symbol (Set ModuleNameBS)
                 -> IO ()
writeSymbolCache cache sm =
  void . withFile cache WriteMode $ \hdl ->
    LBS.hPutStr hdl (encode sm)

-- | Check whether given file is older than any file from the given set.
-- Returns True if given file does not exist.
isOlderThan :: FilePath -> [TimedFile] -> IO Bool
isOlderThan cache files = do
  exist <- doesFileExist cache
  if not exist
  then return True
  else do
    tCache <- getModTime cache
    return $ any (tCache <=) $ map tfTime files -- including equal just in case

-- | Browsing all functions in all system modules.
getGlobalSymbolTable :: HscEnv -> IO (Map Symbol (Set ModuleNameBS))
getGlobalSymbolTable hsc_env =
  foldM (extend hsc_env) M.empty $ listVisibleModules $ hsc_dflags hsc_env

extend :: HscEnv
       -> Map Symbol (Set ModuleNameBS)
       -> Module
       -> IO (Map Symbol (Set ModuleNameBS))
extend hsc_env mm mdl = do
  eps <- readIORef $ hsc_EPS hsc_env
  modinfo <- unsafeInterleaveIO $ runLightGhc hsc_env $ do
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

data AsyncSymbolDb = AsyncSymbolDb FilePath (MVar (Either SomeException SymbolDb))

asyncLoadSymbolDb :: IOish m
                  => FilePath
                  -> MVar (Either SomeException SymbolDb)
                  -> GhcModT m ()
asyncLoadSymbolDb tmpdir mv = void $
    liftBaseWith $ \run -> forkIO $ void $ run $ do
      edb <- gtry $ loadSymbolDb tmpdir
      liftIO $ putMVar mv edb

newAsyncSymbolDb :: IOish m => FilePath -> GhcModT m AsyncSymbolDb
newAsyncSymbolDb tmpdir = do
    mv <- liftIO newEmptyMVar
    asyncLoadSymbolDb tmpdir mv
    return $ AsyncSymbolDb tmpdir mv

getAsyncSymbolDb :: forall m. IOish m => AsyncSymbolDb -> GhcModT m SymbolDb
getAsyncSymbolDb (AsyncSymbolDb tmpdir mv) = do
  db <- liftIO $ handleEx <$> takeMVar mv
  outdated <- isOutdated db
  if outdated
    then do
      asyncLoadSymbolDb tmpdir mv
      liftIO $ handleEx <$> readMVar mv
    else do
      liftIO $ putMVar mv $ Right db
      return db
 where
   handleEx edb =
     case edb of
       Left ex -> throw ex
       Right db -> db
