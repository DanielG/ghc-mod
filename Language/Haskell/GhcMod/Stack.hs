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

module Language.Haskell.GhcMod.Stack where


import Control.Applicative
import Control.Exception as E
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Data.List
import Data.List.Split
import Data.Maybe
import System.Directory
import System.FilePath
import System.Info.Extra
import Exception

import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Monad.Types
import Language.Haskell.GhcMod.Output
import qualified Language.Haskell.GhcMod.Utils as U
import Prelude

patchStackPrograms :: (IOish m, GmOut m) => Cradle -> Programs -> m Programs
patchStackPrograms Cradle { cradleProject = (StackProject senv) } progs = do
  Just ghc <- getStackGhcPath senv
  Just ghcPkg <- getStackGhcPkgPath senv
  return $ progs {
      ghcProgram = ghc
    , ghcPkgProgram = ghcPkg
    }
patchStackPrograms _crdl progs = return progs

getStackEnv :: (IOish m, GmOut m) => FilePath -> m (Maybe StackEnv)
getStackEnv projdir = U.withDirectory_ projdir $ runMaybeT $ do
    env <- map (liToTup . splitOn ": ") . lines <$> readStack ["path"]
    let look k = fromJust $ lookup k env
    return StackEnv {
        seDistDir       = look "dist-dir"
      , seBinPath       = splitSearchPath $ look "bin-path"
      , seSnapshotPkgDb = look "snapshot-pkg-db"
      , seLocalPkgDb    = look "local-pkg-db"
      }
 where
   liToTup [k,v] = (k,v)
   liToTup _ = error "getStackEnv"

getStackGhcPath :: IOish m => StackEnv -> m (Maybe FilePath)
getStackGhcPath = findExecutablesInStackBinPath "ghc"

getStackGhcPkgPath :: IOish m => StackEnv -> m (Maybe FilePath)
getStackGhcPkgPath = findExecutablesInStackBinPath  "ghc-pkg"

findExecutablesInStackBinPath :: IOish m => String -> StackEnv -> m (Maybe FilePath)
findExecutablesInStackBinPath exe StackEnv {..} =
    liftIO $ listToMaybe <$> findExecutablesInDirectories' seBinPath exe

findExecutablesInDirectories' :: [FilePath] -> String -> IO [FilePath]
findExecutablesInDirectories' path binary =
    U.findFilesWith' isExecutable path (binary <.> exeExtension)
   where isExecutable file = do
             perms <- getPermissions file
             return $ executable perms

         exeExtension = if isWindows then "exe" else ""

readStack :: (IOish m, GmOut m) => [String] -> MaybeT m String
readStack args = do
  stack <- MaybeT $ liftIO $ findExecutable "stack"
  readProc <- lift gmReadProcess
  lift $ flip gcatch (\(e :: IOError) -> exToErr e) $ do
    liftIO $ evaluate =<< readProc stack args ""
 where
   exToErr = throw . GMEStackBootstrap . GMEString . show
