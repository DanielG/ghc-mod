-- ghc-mod: Making Haskell development *more* fun
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
{-# LANGUAGE OverloadedStrings, ImplicitParams #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module GHCMod.Options.Commands where

import Options.Applicative
import Options.Applicative.Types
import Options.Applicative.Builder.Internal
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Read
import GHCMod.Options.DocUtils
import GHCMod.Options.Help
import System.FilePath ((</>))

type Symbol = String
type Expr = String
type Module = String
type Line = Int
type Col = Int
type Point = (Line, Col)

data GhcModCommands =
    CmdLang
  | CmdFlag
  | CmdDebug
  | CmdBoot
  | CmdNukeCaches
  | CmdRoot
  | CmdLegacyInteractive FilePath
  | CmdModules Bool
  | CmdDumpSym FilePath
  | CmdFind Symbol
  | CmdDoc Module
  | CmdLint LintOpts FilePath
  | CmdBrowse BrowseOpts [Module]
  | CmdDebugComponent [String]
  | CmdCheck [FilePath]
  | CmdExpand [FilePath]
  | CmdInfo FilePath Symbol
  | CmdType FilePath Point
  | CmdSplit FilePath Point
  | CmdSig FilePath Point
  | CmdAuto FilePath Point
  | CmdRefine FilePath Point Expr
  -- interactive-only commands
  | CmdMapFile FilePath
  | CmdUnmapFile FilePath
  | CmdQuit
  deriving (Show)

commandsSpec :: (?cwd :: FilePath) => Parser GhcModCommands
commandsSpec =
  hsubparser commands

commands :: (?cwd :: FilePath) => Mod CommandFields GhcModCommands
commands =
       command "lang"
          $$  info (pure CmdLang)
          $$  progDesc "List all known GHC language extensions"
    <> command "flag"
          $$  info (pure CmdFlag)
          $$  progDesc "List GHC -f<foo> flags"
    <> command "debug"
          $$  info (pure CmdDebug)
          $$  progDesc' $$$ do
              "Print debugging information. Please include"
                \\ "the output in any bug reports you submit"
    <> command "debug-component"
          $$  info (filesArgsSpec CmdDebugComponent)
          $$  progDesc' $$$ do
              "Debugging information related to cabal component"
                \\ "resolution"
    <> command "boot"
          $$  info (pure CmdBoot)
          $$  progDesc "Internal command used by the emacs frontend"
    -- <> command "nuke-caches"
    --       $$  info (pure CmdNukeCaches) idm
    <> command "root"
          $$  info (pure CmdRoot)
          $$  progDesc'
              "Try to find the project directory."
          <=> desc $$$ do
              "For Cabal projects this is the"
                \\ "directory containing the cabal file, for projects"
                \\ "that use a cabal sandbox but have no cabal file"
                \\ "this is the directory containing the cabal.sandbox.config"
                \\ "file and otherwise this is the current directory"
    <> command "legacy-interactive"
          $$  info (const (CmdLegacyInteractive ?cwd) <$> optional interactiveCommandsSpec)
          $$  progDesc "ghc-modi compatibility mode"
    <> command "list" modulesInfo
    <> command "modules" modulesInfo
    <> command "dumpsym"
          $$  info (CmdDumpSym <$> strArg "TMPDIR") idm
    <> command "find"
          $$  info (CmdFind <$> strArg "SYMBOL")
          $$  progDesc "List all modules that define SYMBOL"
    <> command "doc"
          $$  info (CmdDoc <$> strArg "MODULE")
          $$  progDesc' $$$ do
              "Try finding the html documentation directory"
                \\ "for the given MODULE"
    <> command "lint"
          $$  info (
                CmdLint
                  <$> LintOpts <$$> many $$ strOption
                      $$  long "hlintOpt"
                      <=> short 'h'
                      <=> help "Option to be passed to hlint"
                  <*> fileArg "FILE"
                  )
          $$  progDesc "Check files using `hlint'"
    <> command "browse"
          $$  info (
                CmdBrowse
                  <$> (BrowseOpts
                        <$> switch
                            $$  long "operators"
                            <=> short 'o'
                            <=> help "Also print operators"
                        <*> switch
                            $$  long "detailed"
                            <=> short 'd'
                            <=> help "Print symbols with accompanying signature"
                        <*> switch
                            $$  long "qualified"
                            <=> short 'q'
                            <=> help "Qualify symbols"
                      )
                  <*> some (strArg "MODULE")
                  )
          $$  progDesc "List symbols in a module"
    <> command "check"
          $$  info (filesArgsSpec CmdCheck)
          $$  progDesc' $$$ do
              "Load the given files using GHC and report errors/warnings,"
                \\ "but don't produce output files"
    <> command "expand"
          $$  info (filesArgsSpec CmdExpand)
          $$  progDesc "Like `check' but also pass `-ddump-splices' to GHC"
    <> command "info"
          $$  info (CmdInfo <$> fileArg "FILE" <*> strArg "SYMBOL")
          $$  progDesc' $$$ do
              "Look up an identifier in the context"
                \\ "of FILE (like ghci's `:info')"
    <> command "type"
          $$  info (locArgSpec CmdType)
          $$  progDesc "Get the type of the expression under (LINE,COL)"
    <> command "split"
          $$  info (locArgSpec CmdSplit)
          $$  progDesc
                  "Split a function case by examining a type's constructors"
          <=> desc $$$ do
                "For example given the following code snippet:"
                code $ do
                  "f :: [a] -> a"
                  "f x = _body"
                "would be replaced by:"
                code $ do
                  "f :: [a] -> a"
                  "f [] = _body"
                  "f (x:xs) = _body"
                "(See https://github.com/kazu-yamamoto/ghc-mod/pull/274)"
    <> command "sig"
          $$  info (locArgSpec CmdSig)
          $$  progDesc "Generate initial code given a signature"
          <=> desc $$$ do
              "For example when (LINE,COL) is on the"
                \\ "signature in the following code snippet:"
              code "func :: [a] -> Maybe b -> (a -> b) -> (a,b)"
              "ghc-mod would add the following on the next line:"
              code "func x y z f = _func_body"
              "(See: https://github.com/kazu-yamamoto/ghc-mod/pull/274)"
    <> command "auto"
          $$  info (locArgSpec CmdAuto)
          $$  progDesc "Try to automatically fill the contents of a hole"
    <> command "refine"
          $$  info (locArgSpec CmdRefine <*> strArg "SYMBOL")
          $$  progDesc "Refine the typed hole at (LINE,COL) given EXPR"
          <=> desc $$$ do
              "For example if EXPR is `filter', which has type"
                \\ "`(a -> Bool) -> [a] -> [a]' and (LINE,COL) is on"
                \\ " the hole `_body' in the following code snippet:"
              code $ do
                "filterNothing :: [Maybe a] -> [a]"
                "filterNothing xs = _body"
              "ghc-mod changes the code to get a value of type"
                \\ " `[a]', which results in:"
              code "filterNothing xs = filter _body_1 _body_2"
              "(See also: https://github.com/kazu-yamamoto/ghc-mod/issues/311)"
  where
    modulesInfo =
      info (
        CmdModules <$> switch
              $$  long "detailed"
              <=> short 'd'
              <=> help "Print package modules belong to"
        )
      $ progDesc "List all visible modules"

interactiveCommandsSpec :: (?cwd :: FilePath) => Parser GhcModCommands
interactiveCommandsSpec =
    hsubparser'
        $  commands
        <> command "map-file"
              $$  info (CmdMapFile <$> fileArg "FILE")
              $$  progDesc "tells ghc-modi to read `file.hs` source from stdin"
              <=> desc $$$ do
                "Works the same as second form of"
                  \\ "`--map-file` CLI option."
        <> command "unmap-file"
              $$  info (CmdUnmapFile <$> fileArg "FILE")
              $$  progDesc' $$$ do
                    "unloads previously mapped file,"
                      \\ "so that it's no longer mapped."
              <=> desc $$$ do
                    "`file.hs` can be full path or relative"
                      \\ "to project root, either will work."
        <> command "quit"
              $$ info (pure CmdQuit)
              $$ progDesc "Exit interactive mode"
        <> command ""
              $$ info (pure CmdQuit) idm

strArg :: String -> Parser String
strArg = argument str . metavar

fileArg :: (?cwd :: FilePath) => String -> Parser FilePath
fileArg = fmap (?cwd </>) . argument str . metavar

filesArgsSpec :: (?cwd :: FilePath) => ([String] -> b) -> Parser b
filesArgsSpec x = x <$> some (fileArg "FILES..")

locArgSpec :: (?cwd :: FilePath) => (FilePath -> (Int, Int) -> b) -> Parser b
locArgSpec x = x
  <$> fileArg "FILE"
  <*> ( (,)
        <$> argument int (metavar "LINE")
        <*> argument int (metavar "COL")
      )

hsubparser' :: Mod CommandFields a -> Parser a
hsubparser' m = mkParser d g rdr
  where
    Mod _ d g = m `mappend` metavar ""
    (cmds, subs) = mkCommand m
    rdr = CmdReader cmds (fmap add_helper . subs)
    add_helper pinfo = pinfo
      { infoParser = infoParser pinfo <**> helper }

int :: ReadM Int
int = do
  v <- readerAsk
  maybe (readerError $ "Not a number \"" ++ v ++ "\"") return $ readMaybe v
