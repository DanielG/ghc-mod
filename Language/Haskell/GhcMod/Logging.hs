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

module Language.Haskell.GhcMod.Logging (
    module Language.Haskell.GhcMod.Logging
  , module Language.Haskell.GhcMod.Pretty
  , GmLogLevel(..)
  , module Text.PrettyPrint
  , module Data.Monoid
  ) where

import Control.Monad
import Data.Monoid (mempty, mappend, mconcat, (<>))
import System.IO
import Text.PrettyPrint hiding (style, (<>))

import Language.Haskell.GhcMod.Monad.Types
import Language.Haskell.GhcMod.Pretty

gmSetLogLevel :: GmLog m => GmLogLevel -> m ()
gmSetLogLevel level =
    gmlJournal $ GhcModLog (Just level) []

increaseLogLevel :: GmLogLevel -> GmLogLevel
increaseLogLevel l | l == maxBound = l
increaseLogLevel l = succ l

-- |
-- >>> Just GmDebug <= Nothing
-- False
-- >>> Just GmException <= Just GmDebug
-- True
-- >>> Just GmDebug <= Just GmException
-- False
gmLog :: (MonadIO m, GmLog m) => GmLogLevel -> String -> Doc -> m ()
gmLog level loc' doc = do
  GhcModLog { gmLogLevel = level' } <- gmlHistory

  let loc | loc' == "" = empty
          | otherwise = text (head $ lines loc') <> colon
      msg = gmRenderDoc $ gmLogLevelDoc level <+> loc <+> doc

  when (Just level <= level') $
       liftIO $ hPutStr stderr msg
  gmlJournal (GhcModLog Nothing [(level, render loc, msg)])
