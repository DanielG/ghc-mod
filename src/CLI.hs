{-# LANGUAGE OverloadedStrings #-}
module CLI (
  LogLevel(..),
  Component(..),
  Command(..),
  Options(..),

  run
) where

import Data.Text hiding (empty)
import Options.Applicative

--------------------------------------------------------------------------------
                             -- Types --
--------------------------------------------------------------------------------
data LogLevel
  = Silent
  | Panic
  | Exception
  | Error
  | Warning
  | Info
  | Debug
  | Vomit
    deriving Show

data Component
  = Module Text
  | Path FilePath

data Command
  = Languages
  | Modules
  | Flags
  | Browse
  | Check
  | Expand
  | CDebug
  | DebugComponent
  | CInfo
  | Type
  | Split
  | Sig
  | Refine
  | Auto
  | Find
  | Lint
  | Root
  | Doc
  | DumpSymbols
  | Boot
  | LegacyInteractive
  | Version
    deriving Show

data Options =
    Options { verbosity     :: LogLevel
            , outputLisp    :: Bool
            , lineSeparator :: Text
            , linePrefix    :: (Text,Text)
            , ghcOption     :: [Text]
            , mapFile       :: (FilePath,FilePath)
            , withGhc       :: FilePath
            , withGhcPkg    :: FilePath
            , withCabal     :: FilePath
            , withStack     :: FilePath
            , cmd           :: Command
            }
    deriving Show

--------------------------------------------------------------------------------
                           -- Interface --
--------------------------------------------------------------------------------
run :: IO Options
run =
  execParser (info optionsParser $ fullDesc <> progDesc "ghc-mod")

parseLogLevel :: Int -> LogLevel
parseLogLevel n
  | n <= 0    = Silent
  | n == 1    = Panic
  | n == 2    = Exception
  | n == 3    = Error
  | n == 4    = Warning
  | n == 5    = Info
  | n == 6    = Debug
  | otherwise = Vomit

optionsParser :: Parser Options
optionsParser =
  Options <$> (silent' <|> verbosity')
          <*> toLisp'
          <*> lineSep'
          <*> linePrefix'
          <*> ghcOption'
          <*> mapFile'
          <*> withGhc'
          <*> withGhcPkg'
          <*> withCabal'
          <*> withStack'
          <*> commandsParser

    where
          silent' =
            (\b -> if b then Silent else Error) <$>
              switch (long "silent"
                      <> short 's'
                      <> help "Be silent, set log level to 0")

          verbosity' =
            parseLogLevel <$>
              option auto (long "verbose"
                           <> short 'v'
                           <> help "Increase or set log level. (0-7)"
                           <> value 3)

          toLisp' =
            switch (long "tolisp"
                    <> short 'l'
                    <> help "Format output as an S-Expression")

          lineSep' =
            option auto (long "line-separator"
                         <> short 'b'
                         <> help "Output line separator"
                         <> value "\n")

          linePrefix' =
            ((\[x,y] -> (x,y)) . splitOn ",") <$>
                option auto (long "line-prefix"
                             <> value ","
                             <> help "Output line separator")

          ghcOption' =
            option auto (long "ghc-option"
                         <> value []
                         <> help "Option to be passed to GHC")

          mapFile' =
            ((\[x,y] -> (show x, show y)) . splitOn ",") <$>
              option auto (long "map-file"
                           <> value ","
                           <> help "Redirect one file to another, --map-file \"file1.hs=file2.hs\"")

          withGhc' =
            option auto (long "with-ghc"
                         <> value ""
                         <> help "GHC executable to use")

          withGhcPkg' =
            option auto (long "with-ghc-pkg"
                         <> value ""
                         <> help "ghc-pkg executable to use (only needed when guessing from GHC path fails)")

          withCabal' =
            option auto (long "with-cabal"
                         <> value ""
                         <> help "cabal-install executable to use")

          withStack' =
            option auto (long "with-stack"
                         <> value ""
                         <> help "stack executable to use")

commandsParser :: Parser Command
commandsParser =
  subparser ( command "lang" (
              info (pure Languages) (desc "Display GHC support"))
           <> command "list" (
                 info (pure Modules) (desc "List all modules in this pkg"))
           <> command "modules" (
                 info (pure Modules) (desc "List all modules in this pkg"))
           <> command "flag" (
                 info (pure Flags) (desc "TODO"))
           <> command "browse" (
                 info (pure Browse) (desc "List all exported names for MODULE"))
           <> command "check" (
                 info (pure Check) (desc "Type-check a module"))
           <> command "expand" (
                 info (pure Expand) (desc "TODO") )
           <> command "debug" (
                 info (pure CDebug) (desc "Output ghc-mod debug info: paths, etc."))
           <> command "debug-component" (
                 info (pure DebugComponent) (desc "Output ghc-mod debug info for a module"))
           <> command "info" (
                 info (pure CInfo) (desc "ghci's :info command"))
           <> command "type" (
                 info (pure Type) (desc "ghci's :type command"))
           <> command "split" (
                 info (pure Split) (desc "Performs case-split on target"))
           <> command "sig" (
                 info (pure Sig) (desc "Generate code stub from type signature"))
           <> command "refine" (
                 info (pure Refine) (desc "Attempts to refine target type-holed"))
           <> command "auto" (
                 info (pure Auto) (desc "Attempts to solve target typed-hole"))
           <> command "find" (
                 info (pure Find) (desc "Lists all modules that define SYMBOL"))
           <> command "lint" (
                 info (pure Lint) (desc "Checks files using hlint"))
           <> command "root" (
                 info (pure Root) (desc "Outputs project root"))
           <> command "doc" (
                 info (pure Doc) (desc "Outputs path of HTML docs for MODULE"))
           <> command "dumpsym" (
                 info (pure DumpSymbols) (desc "TODO"))
           <> command "boot" (
                 info (pure Boot) (desc "Outputs a lot of stuff"))
           <> command "legacy-interactive" (
                 info (pure LegacyInteractive) (desc "Opens a REPL mode, like ghc-modi"))
           <> command "version" (
                 info (pure Version) (desc "Outputs ghc-mod version information"))
           )
  where desc msg = fullDesc <> progDesc msg
