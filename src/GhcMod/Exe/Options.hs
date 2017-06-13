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

module GhcMod.Exe.Options (
  parseArgs,
  parseArgsInteractive,
  GhcModCommands(..)
) where

import Options.Applicative
import Options.Applicative.Types

import GhcMod.Exe.Options.Commands
import GhcMod.Exe.Options.ShellParse
import GhcMod.Exe.Version
import GhcMod.Options.DocUtils
import GhcMod.Options.Options
import GhcMod.Types

parseArgs :: IO (Options, GhcModCommands)
parseArgs =
  execParser opts
  where
    opts = info (argAndCmdSpec <**> helpVersion)
           $$  fullDesc
           <=> header "ghc-mod: Happy Haskell Hacking"

parseArgsInteractive :: String -> Either String GhcModCommands
parseArgsInteractive args =
  handle $ execParserPure (prefs idm) opts $ parseCmdLine args
  where
    opts = info interactiveCommandsSpec $$ fullDesc
    handle (Success a) = Right a
    handle (Failure failure) =
          Left $ fst $ renderFailure failure ""
    handle _ = Left "Completion invoked"

helpVersion :: Parser (a -> a)
helpVersion =
      helper
  <*> abortOption (InfoMsg ghcModVersion)
      $$  long "version"
      <=> help "Print the version of the program."
  <*> argument r
      $$  value id
      <=> metavar ""
  where
    r :: ReadM (a -> a)
    r = do
      v <- readerAsk
      case v of
        "help" -> readerAbort ShowHelpText
        "version" -> readerAbort $ InfoMsg ghcModVersion
        _ -> return id

argAndCmdSpec :: Parser (Options, GhcModCommands)
argAndCmdSpec = (,) <$> globalArgSpec <*> commandsSpec
