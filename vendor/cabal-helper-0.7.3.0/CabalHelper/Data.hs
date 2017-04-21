-- cabal-helper: Simple interface to Cabal's configuration state
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

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fforce-recomp #-}
module CabalHelper.Data where

import Control.Monad
import Data.Functor
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Language.Haskell.TH
import System.FilePath
import System.Directory
import System.IO.Temp
import Prelude

withHelperSources :: (FilePath -> IO a) -> IO a
withHelperSources action = withSystemTempDirectory "cabal-helper" $ \dir -> do
    let chdir = dir </> "CabalHelper"
    createDirectory chdir
    forM_ sourceFiles $ \(fn, src) ->
        BS.writeFile (chdir </> fn) $ UTF8.fromString src
    action dir

sourceFiles :: [(FilePath, String)]
sourceFiles =
  [ ("Main.hs",   $(LitE . StringL <$> runIO (UTF8.toString <$> BS.readFile "CabalHelper/Main.hs")))
  , ("Common.hs", $(LitE . StringL <$> runIO (UTF8.toString <$> BS.readFile "CabalHelper/Common.hs")))
  , ("Sandbox.hs",  $(LitE . StringL <$> runIO (UTF8.toString <$> BS.readFile "CabalHelper/Sandbox.hs")))
  , ("Licenses.hs",  $(LitE . StringL <$> runIO (UTF8.toString <$> BS.readFile "CabalHelper/Licenses.hs")))
  , ("Types.hs",  $(LitE . StringL <$> runIO (UTF8.toString <$> BS.readFile "CabalHelper/Types.hs")))
  ]
