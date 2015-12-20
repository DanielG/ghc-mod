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
module GHCMod.Options.ShellEscape (parseCmdLine) where

import Data.Char
import Data.Maybe

isQuote :: Char -> Bool
isQuote = (==) '"'

isEscapeChar :: Char -> Bool
isEscapeChar = (==) '\\'

isEscapable :: Char -> Bool
isEscapable c = any ($ c) [isSpace, isQuote, isEscapeChar]

go :: String -> String -> [String] -> Maybe Char -> [String]
-- result
go [] curarg accargs _ = reverse $ reverse curarg : accargs
-- escaped character
go (esc:c:cl) curarg accargs quote
  | isEscapeChar esc
  = if isEscapable c
    then go cl (c:curarg) accargs quote
    else go (c:cl) (esc:curarg) accargs quote
-- quote character -- opens quotes
go (c:cl) curarg accargs Nothing
  | isQuote c = go cl curarg accargs (Just c)
-- close quotes
go (c:cl) curarg accargs (Just q)
  | c == q = go cl curarg accargs Nothing
go (c:cl) curarg accargs quotes
  -- space separates argumetns outside quotes
  | isSpace c && isNothing quotes
  = if null curarg
      then go cl curarg accargs quotes
      else go cl [] (reverse curarg : accargs) quotes
  -- general character
  | otherwise = go cl (c:curarg) accargs quotes

parseCmdLine :: String -> [String]
parseCmdLine comline = go comline [] [] Nothing
