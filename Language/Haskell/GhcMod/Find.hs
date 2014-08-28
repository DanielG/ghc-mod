{-# LANGUAGE CPP #-}

module Language.Haskell.GhcMod.Find
#ifndef SPEC
  (
    Symbol
  , SymbolDb
  , loadSymbolDb
  , lookupSymbol
  , dumpSymbol
  , findSymbol
  , lookupSym
  )
#endif
  where

import Config (cProjectVersion,cTargetPlatformString)
import Control.Applicative ((<$>))
import Control.Monad (when, void)
import Control.Monad.Error.Class
import Data.Function (on)
import Data.List (groupBy, sort)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import DynFlags (DynFlags(..), systemPackageConfig)
import Exception (handleIO)
import qualified GHC as G
import Language.Haskell.GhcMod.Convert
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Utils
import Language.Haskell.GhcMod.Types
import Name (getOccString)
import System.Directory (doesDirectoryExist, getAppUserDataDirectory, doesFileExist, getModificationTime)
import System.FilePath ((</>), takeDirectory)
import System.IO
import System.Environment

#ifndef MIN_VERSION_containers
#define MIN_VERSION_containers(x,y,z) 1
#endif

#if MIN_VERSION_containers(0,5,0)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
#else
import Data.Map (Map)
import qualified Data.Map as M
#endif

----------------------------------------------------------------

-- | Type of function and operation names.
type Symbol = String
-- | Database from 'Symbol' to \['ModuleString'\].
newtype SymbolDb = SymbolDb (Map Symbol [ModuleString])
    deriving (Show)

----------------------------------------------------------------

-- | When introducing incompatible changes to the 'symbolCache' file format
-- increment this version number.
symbolCacheVersion :: Integer
symbolCacheVersion = 0

-- | Filename of the symbol table cache file.
symbolCache :: String
symbolCache = "ghc-mod-"++ show symbolCacheVersion ++".cache"

packageCache :: String
packageCache = "package.cache"

packageConfDir :: String
packageConfDir = "package.conf.d"

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
lookupSym sym (SymbolDb db) = fromMaybe [] $ M.lookup sym db

---------------------------------------------------------------

-- | Loading a file and creates 'SymbolDb'.
loadSymbolDb :: (IOish m, MonadError GhcModError m) => m SymbolDb
loadSymbolDb = SymbolDb <$> readSymbolDb

-- | Returns the path to the currently running ghc-mod executable. With ghc<7.6
-- this is a guess but >=7.6 uses 'getExecutablePath'.
ghcModExecutable :: IO FilePath
#ifndef SPEC
ghcModExecutable = do
    dir <- getExecutablePath'
    return $ dir </> "ghc-mod"
#else
ghcModExecutable = do _ <- getExecutablePath' -- get rid of unused warning when
                                              -- compiling spec
                      return "dist/build/ghc-mod/ghc-mod"
#endif
 where
    getExecutablePath' :: IO FilePath
# if __GLASGOW_HASKELL__ >= 706
    getExecutablePath' = takeDirectory <$> getExecutablePath
# else
    getExecutablePath' = return ""
# endif

readSymbolDb :: (IOish m, MonadError GhcModError m) => m (Map Symbol [ModuleString])
readSymbolDb = do
    ghcMod <- liftIO ghcModExecutable
    file <- chop <$> readProcess' ghcMod ["dumpsym"]
    M.fromAscList . map conv . lines <$> liftIO (readFile file)
  where
    conv :: String -> (Symbol,[ModuleString])
    conv = read
    chop "" = ""
    chop xs = init xs

----------------------------------------------------------------
-- used 'ghc-mod dumpsym'

getSymbolCachePath :: IOish m => GhcModT m FilePath
getSymbolCachePath = do
    u:_ <- filter (/= GlobalDb) . cradlePkgDbStack <$> cradle
    Just db <- (liftIO . flip resolvePackageDb u) =<< G.getSessionDynFlags
    return db
  `catchError` const (fail "Couldn't find non-global package database for symbol cache")

-- | Dumping a set of ('Symbol',\['ModuleString'\]) to a file
--   if the file does not exist or is invalid.
--   The file name is printed.

dumpSymbol :: IOish m => GhcModT m String
dumpSymbol = do
    dir <- getSymbolCachePath
    let cache = dir </> symbolCache
        pkgdb = dir </> packageCache

    create <- liftIO $ cache `isNewerThan` pkgdb
    when create $ (liftIO . writeSymbolCache cache) =<< getSymbolTable
    return $ unlines [cache]

writeSymbolCache :: FilePath
                 -> [(Symbol,[ModuleString])]
                 -> IO ()
writeSymbolCache cache sm =
  void . withFile cache WriteMode $ \hdl ->
      mapM (hPrint hdl) sm

isNewerThan :: FilePath -> FilePath -> IO Bool
isNewerThan ref file = do
    exist <- doesFileExist ref
    if not exist then
        return True
      else do
        tRef <- getModificationTime ref
        tFile <- getModificationTime file
        return $ tRef <= tFile -- including equal just in case

-- | Browsing all functions in all system/user modules.
getSymbolTable :: IOish m => GhcModT m [(Symbol,[ModuleString])]
getSymbolTable = do
    ghcModules <- G.packageDbModules True
    moduleInfos <- mapM G.getModuleInfo ghcModules
    let modules = do
         m <- ghcModules
         let moduleName = G.moduleNameString $ G.moduleName m
--             modulePkg = G.packageIdString $ G.modulePackageId m
         return moduleName

    return $ collectModules
           $ extractBindings `concatMap` (moduleInfos `zip` modules)

extractBindings :: (Maybe G.ModuleInfo, ModuleString)
                -> [(Symbol, ModuleString)]
extractBindings (Nothing,_)  = []
extractBindings (Just inf,mdlname) =
    map (\name -> (getOccString name, mdlname)) names
  where
    names = G.modInfoExports inf

collectModules :: [(Symbol,ModuleString)]
               -> [(Symbol,[ModuleString])]
collectModules = map tieup . groupBy ((==) `on` fst) . sort
  where
    tieup x = (head (map fst x), map snd x)

--- Copied from ghc module `Packages' unfortunately it's not exported :/
resolvePackageDb :: DynFlags -> GhcPkgDb -> IO (Maybe FilePath)
resolvePackageDb df GlobalDb         = return $ Just (systemPackageConfig df)
resolvePackageDb _  (PackageDb name) = return $ Just name
resolvePackageDb _  UserDb           = handleIO (\_ -> return Nothing) $ do
    appdir <- getAppUserDataDirectory "ghc"
    let dir = appdir </> (target_arch ++ '-':target_os ++ '-':cProjectVersion)
        pkgconf = dir </> packageConfDir
    exist <- doesDirectoryExist pkgconf
    return $ if exist then Just pkgconf else Nothing
  where
    [target_arch,_,target_os] = splitOn "-" cTargetPlatformString
