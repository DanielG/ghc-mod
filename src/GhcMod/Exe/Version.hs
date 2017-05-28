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

module GhcMod.Exe.Version where

import Paths_ghc_mod
import Data.Version (showVersion)
import Config (cProjectVersion)

progVersion :: String -> String
progVersion pf =
    "ghc-mod"++pf++" version " ++ showVersion version ++ " compiled by GHC "
                               ++ cProjectVersion

ghcModVersion :: String
ghcModVersion = progVersion ""

ghcModiVersion :: String
ghcModiVersion = progVersion "i"
