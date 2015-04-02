{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFunctor, DeriveGeneric,
  StandaloneDeriving, DefaultSignatures, FlexibleInstances #-}
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
import Control.Arrow
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
import Distribution.Helper
import Exception (ExceptionMonad)
#if __GLASGOW_HASKELL__ < 708
import qualified MonadUtils as GHC (MonadIO(..))
#endif
import GHC (ModuleName, moduleNameString, mkModuleName)
import PackageConfig (PackageConfig)
import GHC.Generics

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

data Options = Options {
    outputStyle   :: OutputStyle
  -- | Line separator string.
  , lineSeparator :: LineSeparator
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
  } deriving (Show)


-- | A default 'Options'.
defaultOptions :: Options
defaultOptions = Options {
    outputStyle   = PlainStyle
  , lineSeparator = LineSeparator "\0"
  , logLevel      = GmInfo
  , ghcProgram    = "ghc"
  , ghcPkgProgram = "ghc-pkg"
  , cabalProgram  = "cabal"
  , ghcUserOptions= []
  , operators     = False
  , detailed      = False
  , qualified     = False
  , hlintOpts     = []
  }

----------------------------------------------------------------

-- | The environment where this library is used.
data Cradle = Cradle {
  -- | The directory where this library is executed.
    cradleCurrentDir :: FilePath
  -- | The project root directory.
  , cradleRootDir    :: FilePath
  -- | Per-Project temporary directory
  , cradleTempDir    :: FilePath
  -- | The file name of the found cabal file.
  , cradleCabalFile  :: Maybe FilePath
  -- | Package database stack
  , cradlePkgDbStack  :: [GhcPkgDb]
  } deriving (Eq, Show)

----------------------------------------------------------------

-- | GHC package database flags.
data GhcPkgDb = GlobalDb | UserDb | PackageDb String deriving (Eq, Show)

-- | A single GHC command line option.
type GHCOption  = String

-- | An include directory for modules.
type IncludeDir = FilePath

-- | A package name.
type PackageBaseName = String

-- | A package version.
type PackageVersion  = String

-- | A package id.
type PackageId  = String

-- | A package's name, verson and id.
type Package    = (PackageBaseName, PackageVersion, PackageId)

pkgName :: Package -> PackageBaseName
pkgName (n,_,_) = n

pkgVer :: Package -> PackageVersion
pkgVer (_,v,_) = v

pkgId :: Package -> PackageId
pkgId (_,_,i) = i

showPkg :: Package -> String
showPkg (n,v,_) = intercalate "-" [n,v]

showPkgId :: Package -> String
showPkgId (n,v,i) = intercalate "-" [n,v,i]

-- | Haskell expression.
type Expression = String

-- | Module name.
type ModuleString = String

-- | A Module
type Module = [String]


data GmLogLevel = GmPanic
                | GmException
                | GmError
                | GmWarning
                | GmInfo
                | GmDebug
                  deriving (Eq, Ord, Enum, Bounded, Show, Read)

-- | Collection of packages
type PkgDb = (Map Package PackageConfig)

data GmModuleGraph = GmModuleGraph {
      gmgFileMap    :: Map FilePath ModulePath,
      gmgModuleMap  :: Map ModuleName ModulePath,
      gmgGraph      :: Map ModulePath (Set ModulePath)
    } deriving (Eq, Ord, Show, Read, Generic, Typeable)

instance Serialize GmModuleGraph where
    put GmModuleGraph {..} = let
        mpim :: Map ModulePath Integer
        graph :: Map Integer (Set Integer)

        mpim = Map.fromList $
                (Map.keys gmgGraph) `zip` [0..]
        mpToInt :: ModulePath -> Integer
        mpToInt mp = fromJust $ Map.lookup mp mpim

        graph = Map.map (Set.map mpToInt) $ Map.mapKeys mpToInt gmgGraph
        in put (mpim, graph)

    get = do
     (mpim :: Map ModulePath Integer, graph :: Map Integer (Set Integer)) <- get
     let
         swapMap = Map.fromList . map swap . Map.toList
         swap (a,b) = (b,a)
         impm = swapMap mpim
         intToMp i = fromJust $ Map.lookup i impm
         mpGraph :: Map ModulePath (Set ModulePath)
         mpGraph = Map.map (Set.map intToMp) $ Map.mapKeys intToMp graph
         mpFm = Map.fromList $ map (mpPath &&& id) $ Map.keys mpim
         mpMn = Map.fromList $ map (mpModule &&& id) $ Map.keys mpim
     return $ GmModuleGraph mpFm mpMn mpGraph

instance Monoid GmModuleGraph where
    mempty  = GmModuleGraph mempty mempty mempty
    mappend (GmModuleGraph a b c) (GmModuleGraph a' b' c') =
        GmModuleGraph (a <> a') (b <> b') (Map.unionWith Set.union c c')

data GmComponentType = GMCRaw
                     | GMCResolved
data GmComponent (t :: GmComponentType) eps = GmComponent {
      gmcName            :: ChComponentName,
      gmcGhcOpts         :: [GHCOption],
      gmcGhcSrcOpts      :: [GHCOption],
      gmcRawEntrypoints  :: ChEntrypoint,
      gmcEntrypoints     :: eps,
      gmcSourceDirs      :: [FilePath],
      gmcHomeModuleGraph :: GmModuleGraph
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
    readsPrec d r = readParen (d > app_prec)
                         (\r' -> [(mkModuleName m,t) |
                                 ("ModuleName",s) <- lex r',
                                 (m,t) <- readsPrec (app_prec+1) s]) r
        where app_prec = 10

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
    noMsg = GMENoMsg
    strMsg = GMEString

instance Exception GhcModError

data GMConfigStateFileError
    = GMConfigStateFileNoHeader
    | GMConfigStateFileBadHeader
    | GMConfigStateFileNoParse
    | GMConfigStateFileMissing
--    | GMConfigStateFileBadVersion PackageIdentifier PackageIdentifier (Either ConfigStateFileError LocalBuildInfo)
  deriving (Eq, Show, Read, Typeable)


deriving instance Generic Version
instance Serialize Version

instance Serialize Programs
instance Serialize ChModuleName
instance Serialize ChComponentName
instance Serialize ChEntrypoint
