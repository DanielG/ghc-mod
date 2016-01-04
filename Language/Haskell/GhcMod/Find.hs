{-# LANGUAGE CPP, BangPatterns, DoAndIfThenElse #-}

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
import Language.Haskell.GhcMod.PathsAndFiles
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
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import System.Directory
import System.Directory.ModTime
import System.FilePath ((</>))
import System.IO
import Prelude

----------------------------------------------------------------

-- | Type of function and operation names.
type Symbol = String
-- | Database from 'Symbol' to \['ModuleString'\].
data SymbolDb = SymbolDb
  { table             :: Map Symbol [ModuleString]
  , symbolDbCachePath :: FilePath
  } deriving (Show)

isOutdated :: IOish m => SymbolDb -> GhcModT m Bool
isOutdated db =
  (liftIO . isOlderThan (symbolDbCachePath db)) =<< timedPackageCaches

----------------------------------------------------------------

-- | Looking up 'SymbolDb' with 'Symbol' to \['ModuleString'\]
--   which will be concatenated. 'loadSymbolDb' is called internally.
findSymbol :: IOish m => Symbol -> GhcModT m String
findSymbol sym = do
  tmpdir <- cradleTempDir <$> cradle
  loadSymbolDb tmpdir >>= lookupSymbol sym

-- | Looking up 'SymbolDb' with 'Symbol' to \['ModuleString'\]
--   which will be concatenated.
lookupSymbol :: IOish m => Symbol -> SymbolDb -> GhcModT m String
lookupSymbol sym db = convert' $ lookupSym sym db

lookupSym :: Symbol -> SymbolDb -> [ModuleString]
lookupSym sym db = M.findWithDefault [] sym $ table db

---------------------------------------------------------------

-- | Loading a file and creates 'SymbolDb'.
loadSymbolDb :: IOish m => FilePath -> GhcModT m SymbolDb
loadSymbolDb dir = do
  ghcMod <- liftIO ghcModExecutable
  readProc <- gmReadProcess
  file   <- liftIO $ chop <$> readProc ghcMod ["dumpsym", dir] ""
  !db    <- M.fromAscList . map conv . lines <$> liftIO (readFile file)
  return $ SymbolDb
    { table             = db
    , symbolDbCachePath = file
    }
  where
    conv :: String -> (Symbol, [ModuleString])
    conv = read
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
  runGmPkgGhc $ do
    when create $
      liftIO . writeSymbolCache cache =<< getGlobalSymbolTable
    return $ unlines [cache]
  where
    cache = dir </> symbolCacheFile

writeSymbolCache :: FilePath
                 -> [(Symbol, [ModuleString])]
                 -> IO ()
writeSymbolCache cache sm =
  void . withFile cache WriteMode $ \hdl ->
    mapM (hPrint hdl) sm

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
