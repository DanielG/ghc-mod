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
  -- | Whether or not Template Haskell should be expanded.
  , expandSplice  :: Bool
  -- | The sandbox directory.
  , sandbox       :: Maybe FilePath
  -- | Line separator string.
  , lineSeparator :: LineSeparator
  }

-- | A default 'Options'.
defaultOptions :: Options
defaultOptions = Options {
    outputStyle   = PlainStyle
  , hlintOpts     = []
  , ghcOpts       = []
  , operators     = False
  , detailed      = False
  , expandSplice  = False
  , sandbox       = Nothing
  , lineSeparator = LineSeparator "\0"
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
  -- | The sandbox directory. (e.g. \"\/foo\/bar\/packages-\<ver\>.conf/\")
  , cradlePackageConf :: Maybe FilePath
  } deriving (Eq, Show)

----------------------------------------------------------------

-- | A single GHC option, as it would appear on the command line.
type GHCOption  = String

type IncludeDir = FilePath
type Package    = String

-- | GHC version in 'String'.
type GHCVersion = String

-- | Haskell expression.
type Expression = String

-- | Module name.
type ModuleString = String

data CheckSpeed = Slow | Fast
