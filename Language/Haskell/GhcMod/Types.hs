{-# LANGUAGE FlexibleInstances #-}

module Language.Haskell.GhcMod.Types where

-- | Output style.
data OutputStyle = LispStyle  -- ^ S expression style.
                 | PlainStyle -- ^ Plain textstyle.

-- | The type for line separator. Historically, a Null string is used.
newtype LineSeparator = LineSeparator String

data Options = Options {
    outputStyle   :: OutputStyle
  , hlintOpts     :: [String]
  , ghcOpts       :: [String]
  -- | If 'True', 'browse' also returns operators.
  , operators     :: Bool
  -- | If 'True', 'browse' also returns types.
  , detailed      :: Bool
  -- | If 'True', 'browse' will return fully qualified name
  , qualified     :: Bool
  -- | Whether or not Template Haskell should be expanded.
  , expandSplice  :: Bool
  -- | Line separator string.
  , lineSeparator :: LineSeparator
  -- | Package id of module
  , packageId :: Maybe String
  }

-- | A default 'Options'.
defaultOptions :: Options
defaultOptions = Options {
    outputStyle   = PlainStyle
  , hlintOpts     = []
  , ghcOpts       = []
  , operators     = False
  , detailed      = False
  , qualified     = False
  , expandSplice  = False
  , lineSeparator = LineSeparator "\0"
  , packageId     = Nothing
  }

----------------------------------------------------------------

convert :: ToString a => Options -> a -> String
convert Options{ outputStyle = LispStyle  } = toLisp
convert Options{ outputStyle = PlainStyle } = toPlain

class ToString a where
    toLisp  :: a -> String
    toPlain :: a -> String

instance ToString [String] where
    toLisp  = addNewLine . toSexp True
    toPlain = unlines

instance ToString [((Int,Int,Int,Int),String)] where
    toLisp  = addNewLine . toSexp False . map toS
      where
        toS x = "(" ++ tupToString x ++ ")"
    toPlain = unlines . map tupToString

toSexp :: Bool -> [String] -> String
toSexp False ss = "(" ++ unwords ss ++ ")"
toSexp True ss  = "(" ++ unwords (map quote ss) ++ ")"

tupToString :: ((Int,Int,Int,Int),String) -> String
tupToString ((a,b,c,d),s) = show a ++ " "
                         ++ show b ++ " "
                         ++ show c ++ " "
                         ++ show d ++ " "
                         ++ quote s

quote :: String -> String
quote x = "\"" ++ x ++ "\""

addNewLine :: String -> String
addNewLine = (++ "\n")

----------------------------------------------------------------

-- | The environment where this library is used.
data Cradle = Cradle {
  -- | The directory where this library is executed.
    cradleCurrentDir  :: FilePath
  -- | The directory where a cabal file is found.
  , cradleCabalDir    :: Maybe FilePath
  -- | The file name of the found cabal file.
  , cradleCabalFile   :: Maybe FilePath
  -- | The package db options. ([\"-no-user-package-db\",\"-package-db\",\"\/foo\/bar\/i386-osx-ghc-7.6.3-packages.conf.d\"])
  , cradlePackageDbOpts :: [GHCOption]
  , cradlePackages :: [Package]
  } deriving (Eq, Show)

----------------------------------------------------------------

-- | A single GHC command line option.
type GHCOption  = String

-- | An include directory for modules.
type IncludeDir = FilePath

-- | A package name.
type PackageBaseName = String

-- | A package name and its ID.
type Package    = (PackageBaseName, Maybe String)

-- | Haskell expression.
type Expression = String

-- | Module name.
type ModuleString = String

data CheckSpeed = Slow | Fast

-- | Option information for GHC
data CompilerOptions = CompilerOptions {
    ghcOptions  :: [GHCOption]  -- ^ Command line options
  , includeDirs :: [IncludeDir] -- ^ Include directories for modules
  , depPackages :: [Package]    -- ^ Dependent package names
  } deriving (Eq, Show)
