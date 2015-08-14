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
  )
#endif
  where

import Control.Applicative
import Control.Monad (when, void)
import Data.Function (on)
import Data.List (groupBy, sort)
import qualified GHC as G
import Language.Haskell.GhcMod.Convert
import Language.Haskell.GhcMod.Gap (listVisibleModules)
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.PathsAndFiles
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Utils
import Language.Haskell.GhcMod.World (timedPackageCaches)
import Language.Haskell.GhcMod.Output
import Name (getOccString)
import Module (moduleName)
import System.Directory (doesFileExist, getModificationTime)
import System.FilePath ((</>))
import System.IO
import Prelude

import Data.Map (Map)
import qualified Data.Map as M

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
    tCache <- getModificationTime cache
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
