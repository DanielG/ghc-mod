-- ghc-mod: Happy Haskell Hacking
-- Copyright (C) 2015  Nikolay Yakimov <root@livid.pp.ru>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module GhcMod.Options.Options (
    globalArgSpec
  , parseCmdLineOptions
) where

import Options.Applicative
import Options.Applicative.Types
import GhcMod.Types
import Control.Arrow
import Data.Char (toUpper, toLower)
import Data.List (intercalate)
import GhcMod.Read
import GhcMod.Options.DocUtils
import GhcMod.Options.Help
import Data.Monoid
import Prelude

-- | Parse a set of arguments according to the ghc-mod CLI flag spec, producing
-- @Options@ set accordingly.
parseCmdLineOptions :: [String] -> Maybe Options
parseCmdLineOptions = getParseResult . execParserPure (prefs mempty) (info globalArgSpec mempty)

splitOn :: Eq a => a -> [a] -> ([a], [a])
splitOn c = second (drop 1) . break (==c)

logLevelParser :: Parser GmLogLevel
logLevelParser =
    logLevelSwitch <*>
           logLevelOption
      <||> silentSwitch
  where
    logLevelOption =
      option parseLL
        $$  long "verbose"
        <=> metavar "LEVEL"
        <=> value GmWarning
        <=> showDefaultWith showLL
        <=> help' $$$ do
          "Set log level ("
            <> int' (fromEnum (minBound :: GmLogLevel))
            <> "-"
            <> int' (fromEnum (maxBound :: GmLogLevel))
            <> ")"
          "You can also use strings (case-insensitive):"
          para'
            $ intercalate ", "
            $ map showLL ([minBound..maxBound] :: [GmLogLevel])
    logLevelSwitch =
      repeatAp succ' . length <$> many $$ flag' ()
        $$  short 'v'
        <=> help "Increase log level"
    silentSwitch = flag' GmSilent
        $$  long "silent"
        <=> short 's'
        <=> help "Be silent, set log level to 'silent'"
    showLL = drop 2 . map toLower . show
    repeatAp f n = foldr (.) id (replicate n f)
    succ' x | x == maxBound = x
            | otherwise     = succ x
    parseLL = do
      v <- readerAsk
      let
        il'= toEnum . min maxBound <$> readMaybe v
        ll' = readMaybe ("Gm" ++ capFirst v)
      maybe (readerError $ "Not a log level \"" ++ v ++ "\"") return $ ll' <|> il'
    capFirst (h:t) = toUpper h : map toLower t
    capFirst [] = []

outputOptsSpec :: Parser OutputOpts
outputOptsSpec = OutputOpts
      <$> logLevelParser
      <*> flag PlainStyle LispStyle
          $$  long "tolisp"
          <=> short 'l'
          <=> help "Format output as an S-Expression"
      <*> LineSeparator <$$> strOption
          $$  long "boundary"
          <=> long "line-separator"
          <=> short 'b'
          <=> metavar "SEP"
          <=> value "\0"
          <=> showDefault
          <=> help "Output line separator"
      <*> optional $$ splitOn ',' <$$> strOption
          $$  long "line-prefix"
          <=> metavar "OUT,ERR"
          <=> help "Output prefixes"

programsArgSpec :: Parser Programs
programsArgSpec = Programs
      <$> strOption
          $$  long "with-ghc"
          <=> value "ghc"
          <=> showDefault
          <=> help "GHC executable to use"
      <*> strOption
          $$  long "with-ghc-pkg"
          <=> value "ghc-pkg"
          <=> showDefault
          <=> help "ghc-pkg executable to use (only needed when guessing from GHC path fails)"
      <*> strOption
          $$  long "with-cabal"
          <=> value "cabal"
          <=> showDefault
          <=> help "cabal-install executable to use"
      <*> strOption
          $$  long "with-stack"
          <=> value "stack"
          <=> showDefault
          <=> help "stack executable to use"

-- | An optparse-applicative @Parser@ sepcification for @Options@ so that
-- applications making use of the ghc-mod API can have a consistent way of
-- parsing global options.
globalArgSpec :: Parser Options
globalArgSpec = Options
      <$> outputOptsSpec
      <*> programsArgSpec
      <*> many $$ strOption
          $$  long "ghcOpt"
          <=> long "ghc-option"
          <=> short 'g'
          <=> metavar "OPT"
          <=> help "Option to be passed to GHC"
      <*> many fileMappingSpec
      <*> strOption
          $$  long "encoding"
          <=> value "UTF-8"
          <=> showDefault
          <=> help "I/O encoding"
      <*> switch
          $$  long "stack-build-deps"
          <=> showDefault
          <=> help "Build dependencies if needed when using stack"
  where
    fileMappingSpec =
      getFileMapping . splitOn '=' <$> strOption
        $$  long "map-file"
        <=> metavar "MAPPING"
        <=> fileMappingHelp
    fileMappingHelp = help' $ do
        "Redirect one file to another"
        "--map-file \"file1.hs=file2.hs\""
        indent 4 $ do
          "can be used to tell ghc-mod"
            \\ "that it should take source code"
            \\ "for `file1.hs` from `file2.hs`."
          "`file1.hs` can be either full path,"
            \\ "or path relative to project root."
          "`file2.hs` has to be either relative to project root,"
            \\  "or full path (preferred)"
        "--map-file \"file.hs\""
        indent 4 $ do
          "can be used to tell ghc-mod that it should take"
            \\ "source code for `file.hs` from stdin. File end"
            \\ "marker is `\\n\\EOT\\n`, i.e. `\\x0A\\x04\\x0A`."
            \\ "`file.hs` may or may not exist, and should be"
            \\ "either full path, or relative to project root."
    getFileMapping = second (\i -> if null i then Nothing else Just i)
