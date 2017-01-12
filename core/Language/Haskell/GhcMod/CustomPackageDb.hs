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
module Language.Haskell.GhcMod.CustomPackageDb where

import Control.Applicative
import Control.Monad
import Control.Category ((.))
import Data.Maybe
import Data.Traversable
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Monad.Types
import Language.Haskell.GhcMod.PathsAndFiles
import Prelude hiding ((.))

parseCustomPackageDb :: String -> [GhcPkgDb]
parseCustomPackageDb src = map parsePkgDb $ filter (not . null) $ lines src
 where
   parsePkgDb "global" = GlobalDb
   parsePkgDb "user" = UserDb
   parsePkgDb s = PackageDb s

getCustomPkgDbStack :: (MonadIO m, GmEnv m) => m (Maybe [GhcPkgDb])
getCustomPkgDbStack = do
    mCusPkgDbFile <- liftIO . (traverse readFile <=< findCustomPackageDbFile) . cradleRootDir =<< cradle
    return $ parseCustomPackageDb <$> mCusPkgDbFile
