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

{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}
module GhcMod.HomeModuleGraph (
   GmModuleGraph(..)
 , ModulePath(..)
 , mkFileMap
 , mkModuleMap
 , mkMainModulePath
 , findModulePath
 , findModulePathSet
 , fileModuleName
 , canonicalizeModulePath
 , homeModuleGraph
 , updateHomeModuleGraph
 , canonicalizeModuleGraph
 , reachable
 , moduleGraphToDot
 ) where

import DriverPipeline
import DynFlags
import ErrUtils
import Exception
import Finder
import GHC
import HscTypes
import Pretty

import Control.Arrow ((&&&))
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.State.Strict (execStateT)
import Control.Monad.State.Class
import Data.Maybe
import Data.Monoid as Monoid
import Data.Map  (Map)
import qualified Data.Map  as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.FilePath
import System.Directory
import System.IO
import Prelude

import GhcMod.Logging
import GhcMod.Logger
import GhcMod.Monad.Types
import GhcMod.Types
import GhcMod.Utils (withMappedFile)
import GhcMod.Gap (parseModuleHeader)

-- | Turn module graph into a graphviz dot file
--
-- @dot -Tpng -o modules.png modules.dot@
moduleGraphToDot :: GmModuleGraph -> String
moduleGraphToDot GmModuleGraph { gmgGraph } =
    "digraph {\n" ++ concatMap edges (Map.toList graph) ++ "}\n"
 where
   graph = Map.map (Set.mapMonotonic mpPath)
         $ Map.mapKeysMonotonic mpPath gmgGraph
   edges :: (FilePath, (Set FilePath)) -> String
   edges (f, sf) =
       concatMap (\f' -> "    \""++ f ++"\" -> \""++ f' ++"\"\n") (Set.toList sf)

data S = S {
      sErrors   :: [(ModulePath, ErrorMessages)],
      sWarnings :: [(ModulePath, WarningMessages)],
      sGraph    :: GmModuleGraph
}

defaultS :: S
defaultS = S [] [] mempty

putErr :: MonadState S m
       => (ModulePath, ErrorMessages) -> m ()
putErr e = do
  s <- get
  put s { sErrors = e:sErrors s}

putWarn :: MonadState S m
       => (ModulePath, ErrorMessages) -> m ()
putWarn w = do
  s <- get
  put s { sWarnings = w:sWarnings s}

gmgLookupMP :: MonadState S m => ModulePath -> m (Maybe (Set ModulePath))
gmgLookupMP k = (Map.lookup k . gmgGraph . sGraph) `liftM` get

graphUnion :: MonadState S m => GmModuleGraph -> m ()
graphUnion gmg = do
  s <- get
  put s { sGraph = sGraph s `mappend` gmg }

reachable :: Set ModulePath -> GmModuleGraph -> Set ModulePath
reachable smp0 GmModuleGraph {..} = go smp0
 where
   go smp = let
       δsmp = Set.unions $
                collapseMaybeSet . flip Map.lookup gmgGraph <$> Set.toList smp
       smp' = smp `Set.union` δsmp
    in if smp == smp' then smp' else go smp'

pruneUnreachable :: Set ModulePath -> GmModuleGraph -> GmModuleGraph
pruneUnreachable smp0 gmg@GmModuleGraph {..} = let
    r = reachable smp0 gmg
  in
    GmModuleGraph {
      gmgGraph = Map.filterWithKey (\k _ -> k `Set.member` r) gmgGraph
    }

collapseMaybeSet :: Maybe (Set a) -> Set a
collapseMaybeSet = maybe Set.empty id

homeModuleGraph :: (IOish m, Gm m)
    => HscEnv -> Set ModulePath -> m GmModuleGraph
homeModuleGraph env smp = updateHomeModuleGraph env mempty smp smp

mkMainModulePath :: FilePath -> ModulePath
mkMainModulePath = ModulePath (mkModuleName "Main")

findModulePath :: HscEnv -> ModuleName -> IO (Maybe ModulePath)
findModulePath env mn = do
    fmap (ModulePath mn) <$> find env mn

findModulePathSet :: HscEnv -> [ModuleName] -> IO (Set ModulePath)
findModulePathSet env mns = do
    Set.fromList . catMaybes <$> findModulePath env `mapM` mns

find :: MonadIO m => HscEnv -> ModuleName -> m (Maybe FilePath)
find env mn = liftIO $ do
  res <- findHomeModule env mn
  case res of
   -- TODO: handle SOURCE imports (hs-boot stuff): addBootSuffixLocn loc
    Found loc@ModLocation { ml_hs_file = Just _ } _mod ->
        return $ normalise <$> ml_hs_file loc
    _ -> return Nothing


canonicalizeModulePath :: ModulePath -> IO ModulePath
canonicalizeModulePath (ModulePath mn fp) = ModulePath mn <$> canonicalizePath fp

canonicalizeModuleGraph :: MonadIO m => GmModuleGraph -> m GmModuleGraph
canonicalizeModuleGraph GmModuleGraph {..} = liftIO $ do
    GmModuleGraph . Map.fromList <$> mapM fmg (Map.toList gmgGraph)
 where
   fmg (mp, smp) = liftM2 (,) (canonicalizeModulePath mp) (Set.fromList <$> mapM canonicalizeModulePath (Set.toList smp))


updateHomeModuleGraph :: (IOish m, Gm m)
                      => HscEnv
                      -> GmModuleGraph
                      -> Set ModulePath -- ^ Initial set of modules
                      -> Set ModulePath -- ^ Updated set of modules
                      -> m GmModuleGraph
updateHomeModuleGraph env GmModuleGraph {..} smp sump = do
    -- TODO: It would be good if we could retain information about modules that
    -- stop to compile after we've already successfully parsed them at some
    -- point. Figure out a way to delete the modules about to be updated only
    -- after we're sure they won't fail to parse .. or something. Should probably
    -- push this whole prune logic deep into updateHomeModuleGraph'
   (pruneUnreachable smp . sGraph) `liftM` runS (updateHomeModuleGraph' env sump)
 where
   runS = flip execStateT defaultS { sGraph = graph' }
   graph' = GmModuleGraph {
       gmgGraph = Set.foldr Map.delete gmgGraph sump
    }

mkFileMap :: Set ModulePath -> Map FilePath ModulePath
mkFileMap smp = Map.fromList $ map (mpPath &&& id) $ Set.toList smp

mkModuleMap :: Set ModulePath -> Map ModuleName ModulePath
mkModuleMap smp = Map.fromList $ map (mpModule &&& id) $ Set.toList smp

updateHomeModuleGraph'
    :: forall m. (MonadState S m, IOish m, Gm m)
    => HscEnv
    -> Set ModulePath     -- ^ Initial set of modules
    -> m ()
updateHomeModuleGraph' env smp0 = do
    go `mapM_` Set.toList smp0
 where
   go :: ModulePath -> m ()
   go mp = do
     msmp <- gmgLookupMP mp
     case msmp of
       Just _ -> return ()
       Nothing -> do
           smp <- collapseMaybeSet `liftM` step mp

           graphUnion GmModuleGraph {
               gmgGraph = Map.singleton mp smp
            }

           mapM_ go (Set.toList smp)

   step :: ModulePath -> m (Maybe (Set ModulePath))
   step mp = runMaybeT $ do
       (dflags, ppsrc_fn) <- MaybeT preprocess'
       src <- liftIO $ readFile ppsrc_fn
       imports mp src dflags
    where
      preprocess' :: m (Maybe (DynFlags, FilePath))
      preprocess' = do
        let fn = mpPath mp
        ep <- preprocessFile env fn
        case ep of
          Right (_, x) -> return $ Just x
          Left errs -> do
            -- TODO: Remember these and present them as proper errors if this is
            -- the file the user is looking at.
            gmLog GmWarning ("preprocess " ++ show fn) $ Pretty.empty $+$ (vcat $ map text errs)
            return Nothing


   imports :: ModulePath -> String -> DynFlags -> MaybeT m (Set ModulePath)
   imports mp@ModulePath {..} src dflags =
       case parseModuleHeader src dflags mpPath of
         Left err -> do
           putErr (mp, err)
           mzero

         Right (ws, lmdl) -> do
           putWarn (mp, ws)
           let HsModule {..} = unLoc lmdl
               mns = map (unLoc . ideclName)
                   $ filter (isNothing . ideclPkgQual)
                   $ map unLoc hsmodImports
           -- TODO: handle package qualifier "this"
           liftIO $ Set.fromList . catMaybes <$> mapM (findModulePath env) mns

preprocessFile :: (IOish m, GmEnv m, GmState m) =>
  HscEnv -> FilePath -> m (Either [String] ([String], (DynFlags, FilePath)))
preprocessFile env file =
  withLogger' env $ \setDf -> do
    withMappedFile file $ \fn -> do
      let env' = env { hsc_dflags = setDf (hsc_dflags env) }
      liftIO $ preprocess env' (fn, Nothing)

fileModuleName :: (IOish m, GmEnv m, GmState m) =>
  HscEnv -> FilePath -> m (Either [String] (Maybe ModuleName))
fileModuleName env fn = do
    let handler = liftIO . handle (\(_ :: SomeException) -> return $ Right Nothing)
    ep <- preprocessFile env fn
    case ep of
      Left errs -> do
        return $ Left errs
      Right (_warns, (dflags, procdFile)) -> leftM (errBagToStrList env) =<< handler (do
        src <- readFile procdFile
        case parseModuleHeader src dflags procdFile of
          Left errs -> return $ Left errs
          Right (_, lmdl) -> do
            let HsModule {..} = unLoc lmdl
            return $ Right $ unLoc <$> hsmodName)
  where
    leftM f = either (return . Left <=< f) (return . Right)
