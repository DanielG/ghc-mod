-- ghc-mod: Happy Haskell Hacking
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

module GhcMod.Stack where

import Safe
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

import GhcMod.Types
import GhcMod.Monad.Types
import GhcMod.Output
import GhcMod.Logging
import GhcMod.Error
import qualified GhcMod.Utils as U
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

getStackEnv :: (IOish m, GmOut m, GmLog m)
            => FilePath -> FilePath -> m (Maybe StackEnv)
getStackEnv projdir stackProg = U.withDirectory_ projdir $ runMaybeT $ do
    env <- map (liToTup . splitOn ": ") . lines <$> readStack stackProg ["path"]
    let look k = fromJustNote "getStackEnv" $ lookup k env
    return StackEnv {
        seDistDir       = look "dist-dir"
      , seBinPath       = splitSearchPath $ look "bin-path"
      , seSnapshotPkgDb = look "snapshot-pkg-db"
      , seLocalPkgDb    = look "local-pkg-db"
      }
 where
   liToTup [k,v] = (k,v)
   liToTup [k] = (k, error "getStackEnv: missing key '"++k++"'")
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
    U.findFilesWith' isExecutable path (binary <.> exeExtension')
   where isExecutable file = do
             perms <- getPermissions file
             return $ executable perms

         exeExtension' = if isWindows then "exe" else ""

readStack :: (IOish m, GmOut m, GmLog m)
          => FilePath -> [String] -> MaybeT m String
readStack exe args = do
  stack <- MaybeT $ liftIO $ findExecutable exe
  readProc <- lift gmReadProcess
  flip gcatch handler $ do
    liftIO $ evaluate =<< readProc stack args ""
 where
   handler (e :: IOError) = do
     gmLog GmWarning "readStack" $ gmeDoc $ exToErr e
     mzero
   exToErr = GMEStackBootstrap . GMEString . show
