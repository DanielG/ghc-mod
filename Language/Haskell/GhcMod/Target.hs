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

{-# LANGUAGE CPP, ViewPatterns, NamedFieldPuns, RankNTypes #-}
module Language.Haskell.GhcMod.Target where

import Control.Arrow
import Control.Applicative ((<$>))
import Control.Monad.Reader (runReaderT)
import GHC
import GHC.Paths (libdir)
import StaticFlags
import SysTools
import DynFlags
import HscMain
import HscTypes

import Language.Haskell.GhcMod.DynFlags
import Language.Haskell.GhcMod.Monad.Types
import Language.Haskell.GhcMod.CabalHelper
import Language.Haskell.GhcMod.HomeModuleGraph
import Language.Haskell.GhcMod.GhcPkg
import Language.Haskell.GhcMod.Error
import Language.Haskell.GhcMod.Logging
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Utils

import Data.Maybe
import Data.Either
import Data.Foldable (foldrM)
import Data.IORef
import Data.Map (Map)
import qualified Data.Map  as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Distribution.Helper

import System.Directory
import System.FilePath

withLightHscEnv :: forall m a. IOish m
    => [GHCOption] -> (HscEnv -> m a) -> m a
withLightHscEnv opts action = gbracket initEnv teardownEnv action
 where
   teardownEnv :: HscEnv -> m ()
   teardownEnv env = liftIO $ do
       let dflags = hsc_dflags env
       cleanTempFiles dflags
       cleanTempDirs dflags

   initEnv :: m HscEnv
   initEnv = liftIO $ do
     initStaticOpts
     settings <- initSysTools (Just libdir)
     dflags  <- initDynFlags (defaultDynFlags settings)
     env <- newHscEnv dflags
     dflags' <- runLightGhc env $ do
         -- HomeModuleGraph and probably all other clients get into all sorts of
         -- trouble if the package state isn't initialized here
         _ <- setSessionDynFlags =<< getSessionDynFlags
         addCmdOpts opts =<< getSessionDynFlags
     newHscEnv dflags'

runLightGhc :: HscEnv -> LightGhc a -> IO a
runLightGhc env action = do
  renv <- newIORef env
  flip runReaderT renv $ unLightGhc action

runGmPkgGhc :: (IOish m, GmEnv m) => LightGhc a -> m a
runGmPkgGhc action = do
    pkgOpts <- packageGhcOptions
    withLightHscEnv pkgOpts $ \env -> liftIO $ runLightGhc env action

initSession :: IOish m
            => [GHCOption] -> (DynFlags -> Ghc DynFlags) -> GhcModT m ()
initSession opts mdf = do
   s <- gmsGet
   case gmGhcSession s of
     Just GmGhcSession {..} -> do
         if gmgsOptions == opts
            then return ()
            else error "TODO: reload stuff"
     Nothing -> do
       Cradle { cradleTempDir } <- cradle
       ghc <- liftIO $ runGhc (Just libdir) $ do
           let setDf df =
                setTmpDir cradleTempDir <$> (mdf =<< addCmdOpts opts df)
           _ <- setSessionDynFlags =<< setDf =<< getSessionDynFlags
           getSession

       rghc <- liftIO $ newIORef ghc
       gmsPut s { gmGhcSession = Just $ GmGhcSession opts rghc }


-- $ do
--         dflags <- getSessionDynFlags
--         defaultCleanupHandler dflags $ do
--             initializeFlagsWithCradle opt (gmCradle env)
--

-- initSession :: GhcMonad m => Options -> [GHCOption] -> m ()
-- initSession Options {..} ghcOpts = do
--     df <- G.getSessionDynFlags
--     void $
--       ( setModeSimple -- $ setEmptyLogger
--                       df)

runGmlT :: IOish m => [Either FilePath ModuleName] -> GmlT m a -> GhcModT m a
runGmlT fns action = runGmlT' fns return action

runGmlT' :: IOish m
              => [Either FilePath ModuleName]
              -> (DynFlags -> Ghc DynFlags)
              -> GmlT m a
              -> GhcModT m a
runGmlT' fns mdf action = runGmlTWith fns mdf id action

runGmlTWith :: IOish m
                 => [Either FilePath ModuleName]
                 -> (DynFlags -> Ghc DynFlags)
                 -> (GmlT m a -> GmlT m b)
                 -> GmlT m a
                 -> GhcModT m b
runGmlTWith efnmns' mdf wrapper action = do
    crdl <- cradle
    Options { ghcUserOptions } <- options

    let (fns, mns) = partitionEithers efnmns'
        ccfns = map (cradleCurrentDir crdl </>) fns
    cfns <- liftIO $ mapM canonicalizePath ccfns
    let rfns = map (makeRelative $ cradleRootDir crdl) cfns
        serfnmn = Set.fromList $ map Right mns ++ map Left rfns

    opts <- targetGhcOptions crdl serfnmn
    let opts' = opts ++ ghcUserOptions

    initSession opts' $
        setModeSimple >>> setEmptyLogger >>> mdf

    unGmlT $ wrapper $ do
      loadTargets (map moduleNameString mns ++ rfns)
      action

targetGhcOptions :: IOish m
                  => Cradle
                  -> Set (Either FilePath ModuleName)
                  -> GhcModT m [GHCOption]
targetGhcOptions crdl sefnmn = do
    when (Set.null sefnmn) $ error "targetGhcOptions: no targets given"

    case cradleCabalFile crdl of
      Just _ -> cabalOpts
      Nothing -> sandboxOpts crdl
 where
   zipMap f l = l `zip` (f `map` l)
   cabalOpts = do
       mcs <- resolveGmComponents Nothing =<< getComponents

       let mdlcs = moduleComponents mcs `zipMap` Set.toList sefnmn
           candidates = Set.unions $ map snd mdlcs

       let noCandidates = Set.null candidates
           noModuleHasAnyAssignment = all (Set.null . snd) mdlcs

       if noCandidates && noModuleHasAnyAssignment
          then do
            gmLog GmWarning "" $ strDoc $ "Could not find a componenet assignment, falling back to sandbox only project options."
            sandboxOpts crdl
          else do
            when noCandidates $
              throwError $ GMECabalCompAssignment mdlcs

            let cn = pickComponent candidates
            return $ gmcGhcOpts $ fromJust $ Map.lookup cn mcs

moduleComponents :: Map ChComponentName (GmComponent (Set ModulePath))
                 -> Either FilePath ModuleName
                 -> Set ChComponentName
moduleComponents m efnmn =
    foldr' Set.empty m $ \c s ->
        let
            memb =
              case efnmn of
                Left fn  -> fn `Set.member` Set.map mpPath (smp c)
                Right mn -> mn `Set.member` Set.map mpModule (smp c)
        in if memb
           then Set.insert (gmcName c) s
           else s
 where
   smp c = Map.keysSet $ gmgGraph $ gmcHomeModuleGraph c

   foldr' b as f = Map.foldr f b as

pickComponent :: Set ChComponentName -> ChComponentName
pickComponent scn = Set.findMin scn

packageGhcOptions :: (MonadIO m, GmEnv m) => m [GHCOption]
packageGhcOptions = do
    crdl <- cradle
    case cradleCabalFile crdl of
      Just _ -> do
        (Set.toList . Set.fromList . concat . map snd) `liftM` getGhcPkgOptions
      Nothing -> sandboxOpts crdl

sandboxOpts :: Monad m => Cradle -> m [String]
sandboxOpts crdl = return $ ["-i" ++ d | d <- [wdir,rdir]] ++ pkgOpts
  where
    pkgOpts = ghcDbStackOpts $ cradlePkgDbStack crdl
    (wdir, rdir) = (cradleCurrentDir crdl, cradleRootDir crdl)

resolveGmComponent :: (IOish m, GmLog m, GmEnv m)
    => Maybe [Either FilePath ModuleName] -- ^ Updated modules
    -> GmComponent ChEntrypoint
    -> m (GmComponent (Set ModulePath))
resolveGmComponent mums c@GmComponent {..} =
  withLightHscEnv gmcGhcSrcOpts $ \env -> do
    let srcDirs = gmcSourceDirs
        mg = gmcHomeModuleGraph

    Cradle { cradleRootDir } <- cradle

    eps <- liftIO $ resolveChEntrypoints cradleRootDir gmcEntrypoints
    simp <- liftIO $ resolveEntrypoints env srcDirs eps
    sump <- liftIO $ case mums of
        Nothing -> return simp
        Just ums -> resolveEntrypoints env srcDirs ums

    mg' <- updateHomeModuleGraph env mg simp sump

    return $ c { gmcEntrypoints = simp, gmcHomeModuleGraph = mg' }

resolveEntrypoints :: MonadIO m
    => HscEnv -> [FilePath] -> [Either FilePath ModuleName] -> m (Set ModulePath)
resolveEntrypoints env srcDirs ms =
    liftIO $ Set.fromList . catMaybes <$> resolve `mapM` ms
 where
   resolve :: Either FilePath ModuleName -> IO (Maybe ModulePath)
   resolve (Right mn) = findModulePath env mn
   resolve (Left fn') = do
       mfn <- findFile' srcDirs fn'
       case mfn of
         Nothing -> return Nothing
         Just fn'' -> do
             let fn = normalise fn''
             emn <- fileModuleName env fn
             return $ case emn of
                 Left _ -> Nothing
                 Right mmn -> Just $
                     case mmn of
                       Nothing -> mkMainModulePath fn
                       Just mn -> ModulePath mn fn
   findFile' dirs file =
       mconcat <$> mapM (mightExist . (</>file)) dirs

resolveChEntrypoints ::
  FilePath -> ChEntrypoint -> IO [Either FilePath ModuleName]
resolveChEntrypoints _ (ChLibEntrypoint em om) =
    return $ map (Right . chModToMod) (em ++ om)

resolveChEntrypoints _ (ChExeEntrypoint main om) =
    return $ [Left main] ++ map (Right . chModToMod) om

resolveChEntrypoints srcDir ChSetupEntrypoint = do
  shs <- doesFileExist (srcDir </> "Setup.hs")
  slhs <- doesFileExist (srcDir </> "Setup.lhs")
  return $ case (shs, slhs) of
    (True, _) -> [Left "Setup.hs"]
    (_, True) -> [Left "Setup.lhs"]
    (False, False) -> []

chModToMod :: ChModuleName -> ModuleName
chModToMod (ChModuleName mn) = mkModuleName mn

resolveGmComponents :: (IOish m, GmState m, GmLog m, GmEnv m)
                    => Maybe [Either FilePath ModuleName]
                        -- ^ Updated modules
                    -> [GmComponent ChEntrypoint]
                    -> m (Map ChComponentName (GmComponent (Set ModulePath)))
resolveGmComponents mumns cs = do
    s <- gmsGet
    m' <- foldrM' (gmComponents s) cs $ \c m -> do
        case Map.lookup (gmcName c) m of
          Nothing -> insertUpdated m c
          Just c' -> if same gmcRawEntrypoints c c' && same gmcGhcSrcOpts c c'
                       then return m
                       else insertUpdated m c
    gmsPut s { gmComponents = m' }
    return m'

 where
   foldrM' b fa f = foldrM f b fa
   insertUpdated m c = do
     rc <- resolveGmComponent mumns c
     return $ Map.insert (gmcName rc) rc m

   same :: Eq b
        => (forall a. GmComponent a -> b)
        -> GmComponent c -> GmComponent d -> Bool
   same f a b = (f a) == (f b)


-- | Set the files as targets and load them.
loadTargets :: IOish m => [String] -> GmlT m ()
loadTargets filesOrModules = do
    gmLog GmDebug "loadTargets" $
          text "Loading" <+>: fsep (map text filesOrModules)

    targets <- forM filesOrModules (flip guessTarget Nothing)
    setTargets targets

    mode <- getCompilerMode
    if mode == Intelligent
      then loadTargets' Intelligent
      else do
        mdls <- depanal [] False
        let fallback = needsFallback mdls
        if fallback then do
            resetTargets targets
            setIntelligent
            gmLog GmInfo "loadTargets" $
                text "Switching to LinkInMemory/HscInterpreted (memory hungry)"
            loadTargets' Intelligent
          else
            loadTargets' Simple
  where
    loadTargets' Simple = do
        void $ load LoadAllTargets

    loadTargets' Intelligent = do
        df <- getSessionDynFlags
        void $ setSessionDynFlags (setModeIntelligent df)
        void $ load LoadAllTargets

    resetTargets targets = do
        setTargets []
        void $ load LoadAllTargets
        setTargets targets

    setIntelligent = do
        newdf <- setModeIntelligent <$> getSessionDynFlags
        void $ setSessionDynFlags newdf
        setCompilerMode Intelligent

needsFallback :: ModuleGraph -> Bool
needsFallback = any $ \ms ->
                let df = ms_hspp_opts ms in
                   Opt_TemplateHaskell `xopt` df
                || Opt_QuasiQuotes     `xopt` df
#if __GLASGOW_HASKELL__ >= 708
                || (Opt_PatternSynonyms `xopt` df)
#endif
