{-# LANGUAGE FlexibleInstances #-}

module Language.Haskell.GhcMod.Types where

import Data.List (intercalate)

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
  , packageId     :: Maybe String
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

type Builder = String -> String

-- |
--
-- >>> replace '"' "\\\"" "foo\"bar" ""
-- "foo\\\"bar"
replace :: Char -> String -> String -> Builder
replace _ _  [] = id
replace c cs (x:xs)
  | x == c    = (cs ++) . replace c cs xs
  | otherwise = (x :) . replace c cs xs

inter :: Char -> [Builder] -> Builder
inter _ [] = id
inter c bs = foldr1 (\x y -> x . (c:) . y) bs

convert :: ToString a => Options -> a -> String
convert opt@Options { outputStyle = LispStyle  } x = toLisp  opt x "\n"
convert opt@Options { outputStyle = PlainStyle } x = toPlain opt x "\n"

class ToString a where
    toLisp  :: Options -> a -> Builder
    toPlain :: Options -> a -> Builder

lineSep :: Options -> String
lineSep opt = lsep
  where
    LineSeparator lsep = lineSeparator opt

-- |
--
-- >>> toLisp defaultOptions "fo\"o" ""
-- "\"fo\\\"o\""
-- >>> toPlain defaultOptions "foo" ""
-- "foo"
instance ToString String where
    toLisp  opt = quote opt
    toPlain opt = replace '\n' (lineSep opt)

-- |
--
-- >>> toLisp defaultOptions ["foo", "bar", "ba\"z"] ""
-- "(\"foo\" \"bar\" \"ba\\\"z\")"
-- >>> toPlain defaultOptions ["foo", "bar", "baz"] ""
-- "foo\nbar\nbaz"
instance ToString [String] where
    toLisp  opt = toSexp1 opt
    toPlain opt = inter '\n' . map (toPlain opt)

-- |
--
-- >>> let inp = [((1,2,3,4),"foo"),((5,6,7,8),"bar")] :: [((Int,Int,Int,Int),String)]
-- >>> toLisp defaultOptions inp ""
-- "((1 2 3 4 \"foo\") (5 6 7 8 \"bar\"))"
-- >>> toPlain defaultOptions inp ""
-- "1 2 3 4 \"foo\"\n5 6 7 8 \"bar\""
instance ToString [((Int,Int,Int,Int),String)] where
    toLisp  opt = toSexp2 . map toS
      where
        toS x = ('(' :) . tupToString opt x . (')' :)
    toPlain opt = inter '\n' . map (tupToString opt)

toSexp1 :: Options -> [String] -> Builder
toSexp1 opt ss = ('(' :) . inter ' ' (map (quote opt) ss) . (')' :)

toSexp2 :: [Builder] -> Builder
toSexp2 ss = ('(' :) . (inter ' ' ss) . (')' :)

tupToString :: Options -> ((Int,Int,Int,Int),String) -> Builder
tupToString opt ((a,b,c,d),s) = (show a ++) . (' ' :)
                              . (show b ++) . (' ' :)
                              . (show c ++) . (' ' :)
                              . (show d ++) . (' ' :)
                              . quote opt s -- fixme: quote is not necessary

quote :: Options -> String -> Builder
quote opt str = ("\"" ++) .  (quote' str ++) . ("\"" ++)
  where
    lsep = lineSep opt
    quote' [] = []
    quote' (x:xs)
      | x == '\n' = lsep   ++ quote' xs
      | x == '\\' = "\\\\" ++ quote' xs
      | x == '"'  = "\\\"" ++ quote' xs
      | otherwise = x       : quote' xs

----------------------------------------------------------------

-- | The environment where this library is used.
data Cradle = Cradle {
  -- | The directory where this library is executed.
    cradleCurrentDir :: FilePath
  -- | The project root directory.
  , cradleRootDir    :: FilePath
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

-- | Option information for GHC
data CompilerOptions = CompilerOptions {
    ghcOptions  :: [GHCOption]  -- ^ Command line options
  , includeDirs :: [IncludeDir] -- ^ Include directories for modules
  , depPackages :: [Package]    -- ^ Dependent package names
  } deriving (Eq, Show)
