{-# LANGUAGE CPP, BangPatterns #-}

module Language.Haskell.GhcMod.Find (
    Symbol
  , SymbolDb
  , loadSymbolDb
  , lookupSymbol
  , dumpSymbol
  , findSymbol
  ) where

import Config (cProjectVersion,cTargetPlatformString)
import Control.Applicative ((<$>))
import Control.Exception (handle, SomeException(..))
import Control.Monad (when, void)
import CoreMonad (liftIO)
import Data.Function (on)
import Data.List (groupBy, sort)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import DynFlags (DynFlags(..), systemPackageConfig)
import Exception (handleIO)
import qualified GHC as G
import Language.Haskell.GhcMod.Convert
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Types
import Name (getOccString)
import System.Directory (doesDirectoryExist, getAppUserDataDirectory, doesFileExist, getModificationTime)
import System.FilePath ((</>))
import System.IO
import System.Process (readProcess)

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
type Db = Map Symbol [ModuleString]
-- | Database from 'Symbol' to \['ModuleString'\].
newtype SymbolDb = SymbolDb Db

----------------------------------------------------------------

symbolCache :: String
symbolCache = "ghc-mod.cache"

packageCache :: String
packageCache = "package.cache"

packageConfDir :: String
packageConfDir = "package.conf.d"

----------------------------------------------------------------

-- | Finding modules to which the symbol belong.
findSymbol :: IOish m => Symbol -> GhcModT m String
findSymbol sym = convert' =<< lookupSymbol' sym <$> liftIO loadSymbolDb

lookupSymbol' :: Symbol -> SymbolDb -> [ModuleString]
lookupSymbol' sym (SymbolDb db) = fromMaybe [] (M.lookup sym db)

-- | Looking up 'SymbolDb' with 'Symbol' to \['ModuleString'\]
--   which will be concatenated.
lookupSymbol :: Options -> Symbol -> SymbolDb -> String
lookupSymbol opt sym db = convert opt $ lookupSymbol' sym db

---------------------------------------------------------------

-- | Loading a file and creates 'SymbolDb'.
loadSymbolDb :: IO SymbolDb
loadSymbolDb = SymbolDb <$> readSymbolDb

readSymbolDb :: IO Db
readSymbolDb = handle (\(SomeException _) -> return M.empty) $ do
    file <- chop <$> readProcess "ghc-mod" ["dumpsym"] []
    M.fromAscList . map conv . lines <$> readFile file
  where
    conv :: String -> (Symbol,[ModuleString])
    conv = read
    chop "" = ""
    chop xs = init xs

----------------------------------------------------------------
-- used 'ghc-mod dumpsym'

getPath :: IOish m => GhcModT m (Maybe String)
getPath = do
    df <- G.getSessionDynFlags
    stack <- cradlePkgDbStack . gmCradle <$> ask
    case filter (GlobalDb /=) stack of
        []  -> return Nothing
        u:_ -> liftIO $ resolvePackageDb df u

-- | Dumping a set of ('Symbol',\['ModuleString'\]) to a file
--   if the file does not exist or is invalid.
--   The file name is printed.
dumpSymbol :: IOish m => GhcModT m String
dumpSymbol = do
    mdir <- getPath
    ret <- case mdir of
        Nothing  -> return ""
        Just dir -> do
            let cache = dir </> symbolCache
                pkgdb = dir </> packageCache
            do -- fixme: bracket
                create <- liftIO $ needToCreate cache pkgdb
                when create $ do
                    sm <- getSymbol
                    void . liftIO $ withFile cache WriteMode $ \hdl ->
                        mapM (hPutStrLn hdl . show) sm
                return cache
    return $ ret ++ "\n"

needToCreate :: FilePath -> FilePath -> IO Bool
needToCreate file1 file2 = do
    exist <- doesFileExist file1
    if not exist then
        return True
      else do
        m1 <- getModificationTime file1
        m2 <- getModificationTime file2
        return $ m1 <= m2 -- including equal just in case

-- | Browsing all functions in all system/user modules.
getSymbol :: IOish m => GhcModT m [(Symbol,[ModuleString])]
getSymbol = do
    ms <- G.packageDbModules True
    let ns = map (G.moduleNameString . G.moduleName) ms
    is <- mapM G.getModuleInfo ms
    let symbols = concatMap toNameModule (zip is ns)
    return $ uniquefy symbols

toNameModule :: (Maybe G.ModuleInfo,ModuleString) -> [(Symbol,ModuleString)]
toNameModule (Nothing,_)  = []
toNameModule (Just inf,mdlname) = map (\name -> (getOccString name, mdlname)) names
  where
    names = G.modInfoExports inf

uniquefy :: [(Symbol,ModuleString)] -> [(Symbol,[ModuleString])]
uniquefy = map tieup . groupBy ((==) `on` fst) . sort
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
