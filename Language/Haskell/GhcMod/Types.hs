{-# LANGUAGE DeriveDataTypeable, GADTs, StandaloneDeriving, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Haskell.GhcMod.Types (
    module Language.Haskell.GhcMod.Types
  , module CabalHelper.Types
  , ModuleName
  , mkModuleName
  , moduleNameString
  ) where

import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Error (Error(..))
import Control.Exception (Exception)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Monoid
import Data.Typeable (Typeable)
import Exception (ExceptionMonad)
import MonadUtils (MonadIO)
import GHC (ModuleName, moduleNameString, mkModuleName)
import PackageConfig (PackageConfig)

import CabalHelper.Types

-- | A constraint alias (-XConstraintKinds) to make functions dealing with
-- 'GhcModT' somewhat cleaner.
--
-- Basicially an @IOish m => m@ is a 'Monad' supporting arbitrary 'IO' and
-- exception handling. Usually this will simply be 'IO' but we parametrise it in
-- the exported API so users have the option to use a custom inner monad.
type IOish m = (Functor m, MonadIO m, MonadBaseControl IO m, ExceptionMonad m)

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
--  -- | @ghc@ program name.
--  , ghcProgram    :: FilePath
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
  , logLevel      = GmException
--  , ghcProgram    = "ghc"
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
    } deriving (Eq, Ord, Show, Read, Typeable)

instance Monoid GmModuleGraph where
    mempty  = GmModuleGraph mempty mempty mempty
    mappend (GmModuleGraph a b c) (GmModuleGraph a' b' c') =
        GmModuleGraph (a <> a') (b <> b') (Map.unionWith Set.union c c')

data GmComponent eps = GmComponent {
      gmcName            :: GmComponentName,
      gmcGhcOpts         :: [GHCOption],
      gmcGhcSrcOpts      :: [GHCOption],
      gmcRawEntrypoints  :: Either FilePath [ModuleName],
      gmcEntrypoints     :: eps,
      gmcSourceDirs      :: [FilePath],
      gmcHomeModuleGraph :: GmModuleGraph
    } deriving (Eq, Ord, Show, Read, Typeable)

data ModulePath = ModulePath { mpModule :: ModuleName, mpPath :: FilePath }
    deriving (Eq, Ord, Show, Read, Typeable)

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

    | GMECabalComponent GmComponentName
    -- ^ Cabal component could not be found

    | GMECabalCompAssignment [(Either FilePath ModuleName, Set GmComponentName)]
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
