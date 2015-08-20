{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFunctor, DeriveGeneric,
  StandaloneDeriving, DefaultSignatures, FlexibleInstances, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-deprecations #-}
module Language.Haskell.GhcMod.Types (
    module Language.Haskell.GhcMod.Types
  , ModuleName
  , mkModuleName
  , moduleNameString
  ) where

import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Error (Error(..))
import qualified Control.Monad.IO.Class as MTL
import Control.Exception (Exception)
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Serialize
import Data.Version
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Monoid
import Data.Maybe
import Data.Typeable (Typeable)
import Data.IORef
import Data.Label.Derive
import Distribution.Helper
import Exception (ExceptionMonad)
#if __GLASGOW_HASKELL__ < 708
import qualified MonadUtils as GHC (MonadIO(..))
#endif
import GHC (ModuleName, moduleNameString, mkModuleName)
import HscTypes (HscEnv)
import PackageConfig (PackageConfig)
import GHC.Generics
import Text.PrettyPrint (Doc)
import Prelude

import Language.Haskell.GhcMod.Caching.Types

-- | A constraint alias (-XConstraintKinds) to make functions dealing with
-- 'GhcModT' somewhat cleaner.
--
-- Basicially an @IOish m => m@ is a 'Monad' supporting arbitrary 'IO' and
-- exception handling. Usually this will simply be 'IO' but we parametrise it in
-- the exported API so users have the option to use a custom inner monad.
type IOish m = (Functor m, MonadIO m, MonadBaseControl IO m, ExceptionMonad m)


-- MonadUtils of GHC 7.6 or earlier defines its own MonadIO.
-- MonadUtils of GHC 7.8 or later imports MonadIO in Monad.Control.IO.Class.
#if __GLASGOW_HASKELL__ < 708
type MonadIOC m = (GHC.MonadIO m, MTL.MonadIO m)
#else
type MonadIOC m = (MTL.MonadIO m)
#endif

class MonadIOC m => MonadIO m where
  liftIO :: IO a -> m a

-- | Output style.
data OutputStyle = LispStyle  -- ^ S expression style.
                 | PlainStyle -- ^ Plain textstyle.
                   deriving (Show)

-- | The type for line separator. Historically, a Null string is used.
newtype LineSeparator = LineSeparator String deriving (Show)

data FileMapping =  FileMapping {fmPath :: FilePath, fmTemp :: Bool}
                  deriving Show

type FileMappingMap = Map FilePath FileMapping

data Options = Options {
    outputStyle   :: OutputStyle
  -- | Line separator string.
  , lineSeparator :: LineSeparator
  -- | Stdout/err line multiplexing using prefix encoding. @fst@ is stdout,
  -- @snd@ is stderr prefix.
  , linePrefix :: Maybe (String, String)
  -- | Verbosity
  , logLevel      :: GmLogLevel
  -- | @ghc@ program name.
  , ghcProgram    :: FilePath
  -- | @ghc-pkg@ program name.
  , ghcPkgProgram :: FilePath
  -- | @cabal@ program name.
  , cabalProgram  :: FilePath
    -- | GHC command line options set on the @ghc-mod@ command line
  , ghcUserOptions:: [GHCOption]
  -- | If 'True', 'browse' also returns operators.
  , operators     :: Bool
  -- | If 'True', 'browse' also returns types.
  , detailed      :: Bool
  -- | If 'True', 'browse' will return fully qualified name
  , qualified     :: Bool
  , hlintOpts     :: [String]
  , fileMappings  :: [(FilePath, Maybe FilePath)]
  } deriving (Show)

-- | A default 'Options'.
defaultOptions :: Options
defaultOptions = Options {
    outputStyle    = PlainStyle
  , lineSeparator  = LineSeparator "\0"
  , linePrefix     = Nothing
  , logLevel       = GmWarning
  , ghcProgram     = "ghc"
  , ghcPkgProgram  = "ghc-pkg"
  , cabalProgram   = "cabal"
  , ghcUserOptions = []
  , operators      = False
  , detailed       = False
  , qualified      = False
  , hlintOpts      = []
  , fileMappings   = []
  }

----------------------------------------------------------------

data ProjectType = CabalProject | SandboxProject | PlainProject | StackProject
                 deriving (Eq, Show)

-- | The environment where this library is used.
data Cradle = Cradle {
    cradleProjectType:: ProjectType
  -- | The directory where this library is executed.
  , cradleCurrentDir :: FilePath
  -- | The project root directory.
  , cradleRootDir    :: FilePath
  -- | Per-Project temporary directory
  , cradleTempDir    :: FilePath
  -- | The file name of the found cabal file.
  , cradleCabalFile  :: Maybe FilePath
  -- | The build info directory.
  , cradleDistDir    :: FilePath
  } deriving (Eq, Show)


data GmStream = GmOut | GmErr
                deriving (Show)

data GmLineType = GmTerminated | GmPartial
                deriving (Show)

data GmLines a = GmLines GmLineType a
              deriving (Show, Functor)

unGmLine :: GmLines a -> a
unGmLine (GmLines _ s) = s

data GmOutput = GmOutputStdio
              | GmOutputChan (Chan (GmStream, GmLines String))

data GhcModEnv = GhcModEnv {
      gmOptions    :: Options
    , gmCradle     :: Cradle
    , gmOutput     :: GmOutput
    }

data GhcModLog = GhcModLog {
      gmLogLevel     :: Maybe GmLogLevel,
      gmLogVomitDump :: Last Bool,
      gmLogMessages  :: [(GmLogLevel, String, Doc)]
    } deriving (Show)

instance Monoid GhcModLog where
    mempty = GhcModLog (Just GmPanic) (Last Nothing) mempty
    GhcModLog ml vd ls `mappend` GhcModLog ml' vd' ls' =
        GhcModLog (ml' `mplus` ml) (vd `mappend` vd') (ls `mappend` ls')

data GmGhcSession = GmGhcSession {
      gmgsOptions :: ![GHCOption],
      gmgsSession :: !(IORef HscEnv)
    }

data GhcModCaches = GhcModCaches {
      gmcPackageDbStack   :: CacheContents ChCacheData [GhcPkgDb]
    , gmcMergedPkgOptions :: CacheContents ChCacheData [GHCOption]
    , gmcComponents       :: CacheContents ChCacheData [GmComponent 'GMCRaw ChEntrypoint]
    , gmcResolvedComponents :: CacheContents
          [GmComponent 'GMCRaw (Set.Set ModulePath)]
          (Map.Map ChComponentName (GmComponent 'GMCResolved (Set.Set ModulePath)))
    }

data GhcModState = GhcModState {
      gmGhcSession   :: !(Maybe GmGhcSession)
    , gmComponents   :: !(Map ChComponentName (GmComponent 'GMCResolved (Set ModulePath)))
    , gmCompilerMode :: !CompilerMode
    , gmCaches       :: !GhcModCaches
    , gmMMappedFiles :: !FileMappingMap
    }

data CompilerMode = Simple | Intelligent deriving (Eq,Show,Read)

defaultGhcModState :: GhcModState
defaultGhcModState =
    GhcModState n Map.empty Simple (GhcModCaches n n n n) Map.empty
 where n = Nothing

----------------------------------------------------------------

-- | GHC package database flags.
data GhcPkgDb = GlobalDb
              | UserDb
              | PackageDb String
                deriving (Eq, Show, Generic)

instance Serialize GhcPkgDb

-- | A single GHC command line option.
type GHCOption = String

-- | An include directory for modules.
type IncludeDir = FilePath

-- | A package name.
type PackageBaseName = String

-- | A package version.
type PackageVersion = String

-- | A package id.
type PackageId = String

-- | A package's name, verson and id.
type Package = (PackageBaseName, PackageVersion, PackageId)

pkgName :: Package -> PackageBaseName
pkgName (n, _, _) = n

pkgVer :: Package -> PackageVersion
pkgVer (_, v, _) = v

pkgId :: Package -> PackageId
pkgId (_, _, i) = i

showPkg :: Package -> String
showPkg (n, v, _) = intercalate "-" [n, v]

showPkgId :: Package -> String
showPkgId (n, v, i) = intercalate "-" [n, v, i]

-- | Haskell expression.
newtype Expression = Expression { getExpression :: String }
  deriving (Show, Eq, Ord)

-- | Module name.
newtype ModuleString = ModuleString { getModuleString :: String }
  deriving (Show, Read, Eq, Ord)

data GmLogLevel =
    GmSilent
  | GmPanic
  | GmException
  | GmError
  | GmWarning
  | GmInfo
  | GmDebug
  | GmVomit
    deriving (Eq, Ord, Enum, Bounded, Show, Read)

-- | Collection of packages
type PkgDb = (Map Package PackageConfig)

data GmModuleGraph = GmModuleGraph {
    gmgGraph :: Map ModulePath (Set ModulePath)
  } deriving (Eq, Ord, Show, Read, Generic, Typeable)

instance Serialize GmModuleGraph where
  put GmModuleGraph {..} = put (mpim, graph)
    where
      mpim :: Map ModulePath Integer
      mpim = Map.fromList $ Map.keys gmgGraph `zip` [0..]
      graph :: Map Integer (Set Integer)
      graph = Map.map (Set.map mpToInt) $ Map.mapKeys mpToInt gmgGraph
      mpToInt :: ModulePath -> Integer
      mpToInt mp = fromJust $ Map.lookup mp mpim

  get = do
    (mpim :: Map ModulePath Integer, graph :: Map Integer (Set Integer)) <- get
    let impm = swapMap mpim
        intToMp i = fromJust $ Map.lookup i impm
        mpGraph :: Map ModulePath (Set ModulePath)
        mpGraph = Map.map (Set.map intToMp) $ Map.mapKeys intToMp graph
    return $ GmModuleGraph mpGraph
    where
      swapMap :: (Ord k, Ord v) => Map k v -> Map v k
      swapMap = Map.fromList . map (\(x, y) -> (y, x)) . Map.toList

instance Monoid GmModuleGraph where
  mempty  = GmModuleGraph mempty
  mappend (GmModuleGraph a) (GmModuleGraph a') =
    GmModuleGraph (Map.unionWith Set.union a a')

data GmComponentType = GMCRaw
                     | GMCResolved
data GmComponent (t :: GmComponentType) eps = GmComponent {
    gmcHomeModuleGraph :: GmModuleGraph
  , gmcName            :: ChComponentName
  , gmcGhcOpts         :: [GHCOption]
  , gmcGhcPkgOpts      :: [GHCOption]
  , gmcGhcSrcOpts      :: [GHCOption]
  , gmcGhcLangOpts     :: [GHCOption]
  , gmcRawEntrypoints  :: ChEntrypoint
  , gmcEntrypoints     :: eps
  , gmcSourceDirs      :: [FilePath]
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

instance Serialize eps => Serialize (GmComponent t eps)

data ModulePath = ModulePath { mpModule :: ModuleName, mpPath :: FilePath }
  deriving (Eq, Ord, Show, Read, Generic, Typeable)
instance Serialize ModulePath

instance Serialize ModuleName where
  get = mkModuleName <$> get
  put mn = put (moduleNameString mn)

instance Show ModuleName where
  show mn = "ModuleName " ++ show (moduleNameString mn)

instance Read ModuleName where
  readsPrec d =
    readParen
      (d > app_prec)
      (\r' -> [ (mkModuleName m, t)
              | ("ModuleName", s) <- lex r'
              , (m, t)            <- readsPrec (app_prec + 1) s
              ])
    where
      app_prec = 10

data GhcModError
  = GMENoMsg
  -- ^ Unknown error

  | GMEString String
  -- ^ Some Error with a message. These are produced mostly by
  -- 'fail' calls on GhcModT.

  | GMECabalConfigure GhcModError
  -- ^ Configuring a cabal project failed.

  | GMECabalFlags GhcModError
  -- ^ Retrieval of the cabal configuration flags failed.

  | GMECabalComponent ChComponentName
  -- ^ Cabal component could not be found

  | GMECabalCompAssignment [(Either FilePath ModuleName, Set ChComponentName)]
  -- ^ Could not find a consistent component assignment for modules

  | GMEProcess String [String] (Either (String, String, Int) GhcModError)
  -- ^ Launching an operating system process failed. Fields in
  -- order: command, arguments, (stdout, stderr, exitcode)

  | GMENoCabalFile
  -- ^ No cabal file found.

  | GMETooManyCabalFiles [FilePath]
  -- ^ Too many cabal files found.

  | GMECabalStateFile GMConfigStateFileError
    -- ^ Reading Cabal's state configuration file falied somehow.
    deriving (Eq,Show,Typeable)

instance Error GhcModError where
  noMsg  = GMENoMsg
  strMsg = GMEString

instance Exception GhcModError

data GMConfigStateFileError
  = GMConfigStateFileNoHeader
  | GMConfigStateFileBadHeader
  | GMConfigStateFileNoParse
  | GMConfigStateFileMissing
--  | GMConfigStateFileBadVersion PackageIdentifier PackageIdentifier (Either ConfigStateFileError LocalBuildInfo)
  deriving (Eq, Show, Read, Typeable)


deriving instance Generic Version
instance Serialize Version

instance Serialize Programs
instance Serialize ChModuleName
instance Serialize ChComponentName
instance Serialize ChEntrypoint

mkLabel ''GhcModCaches
mkLabel ''GhcModState
