{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFunctor, DeriveGeneric, RankNTypes,
  StandaloneDeriving, DefaultSignatures, FlexibleInstances, TemplateHaskell,
  GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-deprecations #-}
module GhcMod.Types (
    module GhcMod.Types
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
import Control.DeepSeq
import Data.Binary
import Data.Binary.Generic
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Monoid
import Data.Maybe
import Data.Typeable (Typeable)
import Data.IORef
import Data.Label.Derive
import Distribution.Helper hiding (Programs(..))
import qualified Distribution.Helper as CabalHelper
import Exception (ExceptionMonad)
#if __GLASGOW_HASKELL__ < 708
import qualified MonadUtils as GHC (MonadIO(..))
#endif
import GHC (ModuleName, moduleNameString, mkModuleName)
import HscTypes (HscEnv)
import GHC.Generics
import Pretty (Doc)
import Prelude

import GhcMod.Caching.Types

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
                  deriving (Eq, Show)

type FileMappingMap = Map FilePath FileMapping

data ProgramSource = ProgramSourceUser | ProgramSourceStack

data Programs = Programs {
  -- | @ghc@ program name.
    ghcProgram    :: FilePath
  -- | @ghc-pkg@ program name.
  , ghcPkgProgram :: FilePath
  -- | @cabal@ program name.
  , cabalProgram  :: FilePath
  -- | @stack@ program name.
  , stackProgram   :: FilePath
  } deriving (Show)

data OutputOpts = OutputOpts {
  -- | Verbosity
    ooptLogLevel      :: GmLogLevel
  , ooptStyle         :: OutputStyle
  -- | Line separator string.
  , ooptLineSeparator :: LineSeparator
  -- | Stdout/err line multiplexing using prefix encoding. @fst@ is stdout,
  -- @snd@ is stderr prefix.
  , ooptLinePrefix    :: Maybe (String, String)
  } deriving (Show)

data Options = Options {
    optOutput         :: OutputOpts
  , optPrograms       :: Programs
    -- | GHC command line options set on the @ghc-mod@ command line
  , optGhcUserOptions :: [GHCOption]
  , optFileMappings   :: [(FilePath, Maybe FilePath)]
  , optEncoding       :: String
  , optStackBuildDeps :: Bool
  } deriving (Show)

-- | A default 'Options'.
defaultOptions :: Options
defaultOptions = Options {
    optOutput     = OutputOpts {
      ooptLogLevel       = GmWarning
    , ooptStyle          = PlainStyle
    , ooptLineSeparator  = LineSeparator "\0"
    , ooptLinePrefix     = Nothing
    }
  , optPrograms       = Programs {
      ghcProgram     = "ghc"
    , ghcPkgProgram  = "ghc-pkg"
    , cabalProgram   = "cabal"
    , stackProgram   = "stack"
    }
  , optGhcUserOptions = []
  , optFileMappings   = []
  , optEncoding       = "UTF-8"
  , optStackBuildDeps = False
  }

----------------------------------------------------------------

data Project = CabalProject
             | SandboxProject
             | PlainProject
             | StackProject StackEnv
               deriving (Eq, Show, Ord)

isCabalHelperProject :: Project -> Bool
isCabalHelperProject StackProject {} = True
isCabalHelperProject CabalProject {} = True
isCabalHelperProject _ = False

data StackEnv = StackEnv {
      seDistDir       :: FilePath
    , seBinPath       :: [FilePath]
    , seSnapshotPkgDb :: FilePath
    , seLocalPkgDb    :: FilePath
    } deriving (Eq, Show, Ord)

-- | The environment where this library is used.
data Cradle = Cradle {
    cradleProject    :: Project
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
  } deriving (Eq, Show, Ord)

data GmStream = GmOutStream | GmErrStream
                deriving (Show)

data GhcModEnv = GhcModEnv {
      gmOptions    :: Options
    , gmCradle     :: Cradle
    }

data GhcModOut = GhcModOut {
      gmoOptions :: OutputOpts
    , gmoChan    :: Chan (Either (MVar ()) (GmStream, String))
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
    , gmCaches       :: !GhcModCaches
    , gmMMappedFiles :: !FileMappingMap
    }

defaultGhcModState :: GhcModState
defaultGhcModState =
    GhcModState n (GhcModCaches n n n n) Map.empty
 where n = Nothing

----------------------------------------------------------------

-- | GHC package database flags.
data GhcPkgDb = GlobalDb
              | UserDb
              | PackageDb String
                deriving (Eq, Show, Generic)

instance Binary GhcPkgDb where
    put = ggput . from
    get = to `fmap` ggget

-- | A single GHC command line option.
type GHCOption = String

-- | An include directory for modules.
type IncludeDir = FilePath

-- | Haskell expression.
newtype Expression = Expression { getExpression :: String }
  deriving (Show, Eq, Ord)

-- | Module name.
newtype ModuleString = ModuleString { getModuleString :: String }
  deriving (Show, Eq, Ord, Binary, NFData)

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

data GmModuleGraph = GmModuleGraph {
    gmgGraph :: Map ModulePath (Set ModulePath)
  } deriving (Eq, Ord, Show, Read, Generic, Typeable)

instance Binary GmModuleGraph where
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
      swapMap :: Ord v => Map k v -> Map v k
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

instance Binary eps => Binary (GmComponent t eps) where
    put = ggput . from
    get = to `fmap` ggget

data ModulePath = ModulePath { mpModule :: ModuleName, mpPath :: FilePath }
  deriving (Eq, Ord, Show, Read, Generic, Typeable)
instance Binary ModulePath where
    put = ggput . from
    get = to `fmap` ggget

instance Binary ModuleName where
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

  | GMEStackConfigure GhcModError
  -- ^ Configuring a stack project failed.

  | GMEStackBootstrap GhcModError
    -- ^ Bootstrapping @stack@ environment failed (process exited with failure)

  | GMECabalCompAssignment [(Either FilePath ModuleName, Set ChComponentName)]
  -- ^ Could not find a consistent component assignment for modules

  | GMEProcess String String [String] (Either Int GhcModError)
  -- ^ Launching an operating system process failed. Fields in
  -- order: function, command, arguments, (stdout, stderr, exitcode)

  | GMENoCabalFile
  -- ^ No cabal file found.

  | GMETooManyCabalFiles [FilePath]
  -- ^ Too many cabal files found.

    deriving (Eq,Show,Typeable)

instance Error GhcModError where
  noMsg  = GMENoMsg
  strMsg = GMEString

instance Exception GhcModError

instance Binary CabalHelper.Programs where
    put = ggput . from
    get = to `fmap` ggget
instance Binary ChModuleName where
    put = ggput . from
    get = to `fmap` ggget
instance Binary ChComponentName where
    put = ggput . from
    get = to `fmap` ggget
instance Binary ChEntrypoint where
    put = ggput . from
    get = to `fmap` ggget

-- | Options for "lintWith" function
data LintOpts = LintOpts {
        optLintHlintOpts :: [String]
        -- ^ options that will be passed to hlint executable
      } deriving (Show)

-- | Default "LintOpts" instance
defaultLintOpts :: LintOpts
defaultLintOpts = LintOpts []

-- | Options for "browseWith" function
data BrowseOpts = BrowseOpts {
        optBrowseOperators      :: Bool
        -- ^ If 'True', "browseWith" also returns operators.
      , optBrowseDetailed       :: Bool
        -- ^ If 'True', "browseWith" also returns types.
      , optBrowseParents        :: Bool
        -- ^ If 'True', "browseWith" also returns parents.
      , optBrowseQualified      :: Bool
        -- ^ If 'True', "browseWith" will return fully qualified name
    } deriving (Show)

-- | Default "BrowseOpts" instance
defaultBrowseOpts :: BrowseOpts
defaultBrowseOpts = BrowseOpts False False False False

mkLabel ''GhcModCaches
mkLabel ''GhcModState
mkLabel ''Options
mkLabel ''OutputOpts
mkLabel ''Programs
