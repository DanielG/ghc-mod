-- ghc-mod: Making Haskell development *more* fun
-- Copyright (C) 2015  Daniel Gröber <dxld ÄT darkboxed DOT org>
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

module Common where

import Control.Applicative
import Data.List
import Data.Maybe
import System.Environment
import System.IO

errMsg :: String -> IO ()
errMsg str = do
  prog <- getProgName
  hPutStrLn stderr $ prog ++ ": " ++ str

align :: String -> String -> String -> String
align n an str = let
    h:rest = lines str
    [hm]   = match n h
    rest'  = [ move (hm - rm) r | r <- rest, rm <- match an r]
    in
      unlines (h:rest')
 where
   match p str' = maybeToList $
     fst <$> find ((p `isPrefixOf`) . snd) ([0..] `zip` tails str')
   move i str' | i > 0  = replicate i ' ' ++ str'
   move i str' = drop i str'
