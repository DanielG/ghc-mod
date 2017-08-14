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
module GhcMod.Exe.Options.ShellParse (parseCmdLine) where

import Data.Char
import Data.List

go :: String -> String -> [String] -> Bool -> [String]
-- result
go [] curarg accargs _ = reverse $ reverse curarg : accargs
go (c:cl) curarg accargs quotes
  -- open quotes
  | c == '\STX', not quotes
  = go cl curarg accargs True
  -- close quotes
  | c == '\ETX', quotes
  = go cl curarg accargs False
  -- space separates arguments outside quotes
  | isSpace c, not quotes
  = if null curarg
      then go cl curarg accargs quotes
      else go cl [] (reverse curarg : accargs) quotes
  -- general character
  | otherwise = go cl (c:curarg) accargs quotes

parseCmdLine :: String -> [String]
parseCmdLine comline'
  | Just comline <- stripPrefix "ascii-escape " $ dropWhile isSpace comline'
  = go (dropWhile isSpace comline) [] [] False
parseCmdLine [] = [""]
parseCmdLine comline = words comline
