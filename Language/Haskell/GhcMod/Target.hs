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
import Control.Applicative (Applicative, (<$>))
import Control.Monad.Reader (runReaderT)
import GHC
import GHC.Paths (libdir)
import StaticFlags
import SysTools
import DynFlags
import HscMain
import HscTypes
import Bag (bagToList)

import Language.Haskell.GhcMod.DynFlags
import Language.Haskell.GhcMod.Monad.Types
import Language.Haskell.GhcMod.CabalHelper
import Language.Haskell.GhcMod.HomeModuleGraph
import Language.Haskell.GhcMod.PathsAndFiles
import Language.Haskell.GhcMod.GhcPkg
import Language.Haskell.GhcMod.Error
import Language.Haskell.GhcMod.Logging
import Language.Haskell.GhcMod.Logger
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Utils


import Data.Maybe
import Data.Monoid
import Data.Either
import Data.Foldable (foldrM)
import Data.Traversable (traverse)
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
         _ <- setSessionDynFlags =<< addCmdOpts opts =<< getSessionDynFlags
         getSessionDynFlags
     newHscEnv dflags'

runLightGhc :: HscEnv -> LightGhc a -> IO a
runLightGhc env action = do
  renv <- newIORef env
  flip runReaderT renv $ unLightGhc action

runGmPkgGhc :: (IOish m, GmEnv m, GmLog m) => LightGhc a -> m a
runGmPkgGhc action = do
    pkgOpts <- packageGhcOptions
    withLightHscEnv pkgOpts $ \env -> liftIO $ runLightGhc env action

initSession :: IOish m
            => [GHCOption] -> (DynFlags -> Ghc DynFlags) -> GhcModT m ()
initSession opts mdf = do
   s <- gmsGet
   case gmGhcSession s of
     Just GmGhcSession {..} -> when (gmgsOptions /= opts) $ putNewSession s
     Nothing -> putNewSession s

 where
   putNewSession s = do
     rghc <- (liftIO . newIORef =<< newSession =<< cradle)
     gmsPut s { gmGhcSession = Just $ GmGhcSession opts rghc }

   newSession Cradle { cradleTempDir } = liftIO $ do
     runGhc (Just libdir) $ do
       let setDf df = setTmpDir cradleTempDir <$> (mdf =<< addCmdOpts opts df)
       _ <- setSessionDynFlags =<< setDf =<< getSessionDynFlags
       getSession

dropSession :: IOish m => GhcModT m ()
dropSession = do
  s <- gmsGet
  case gmGhcSession s of
    Just (GmGhcSession _opts ref) -> do
      -- TODO: This is still not enough, there seem to still be references to
      -- GHC's state around afterwards.
      liftIO $ writeIORef ref (error "HscEnv: session was dropped")
      -- Not available on ghc<7.8; didn't really help anyways
      -- liftIO $ setUnsafeGlobalDynFlags (error "DynFlags: session was dropped")


    Nothing -> return ()
  gmsPut s { gmGhcSession = Nothing }

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
    let serfnmn = Set.fromList $ map Right mns ++ map Left cfns
    opts <- targetGhcOptions crdl serfnmn
    let opts' = opts ++ ["-O0"] ++ ghcUserOptions

    initSession opts' $
        setModeSimple >>> setEmptyLogger >>> mdf

    let rfns = map (makeRelative $ cradleRootDir crdl) cfns

    unGmlT $ wrapper $ do
      loadTargets (map moduleNameString mns ++ rfns)
      action

targetGhcOptions :: forall m. IOish m
                  => Cradle
                  -> Set (Either FilePath ModuleName)
                  -> GhcModT m [GHCOption]
targetGhcOptions crdl sefnmn = do
    when (Set.null sefnmn) $ error "targetGhcOptions: no targets given"

    case cradleCabalFile crdl of
      Just _ -> cabalOpts crdl
      Nothing -> sandboxOpts crdl
 where
   zipMap f l = l `zip` (f `map` l)

   cabalOpts :: Cradle -> GhcModT m [String]
   cabalOpts Cradle{..} = do
       comps <- mapM (resolveEntrypoint crdl) =<< getComponents
       mcs <- cached cradleRootDir resolvedComponentsCache comps

       let mdlcs = moduleComponents mcs `zipMap` Set.toList sefnmn
           candidates = findCandidates $ map snd mdlcs

       let noCandidates = Set.null candidates
           noModuleHasAnyAssignment = all (Set.null . snd) mdlcs

       if noCandidates && noModuleHasAnyAssignment
          then do
            gmLog GmWarning "" $ strDoc $ "Could not find a component assignment, falling back to guessed GHC options."
            sandboxOpts crdl
          else do
            when noCandidates $
              throwError $ GMECabalCompAssignment mdlcs

            let cn = pickComponent candidates
            return $ gmcGhcOpts $ fromJust $ Map.lookup cn mcs

resolvedComponentsCache :: IOish m => Cached (GhcModT m)
    [GmComponent GMCRaw (Set.Set ModulePath)]
    (Map.Map ChComponentName (GmComponent GMCResolved (Set.Set ModulePath)))
resolvedComponentsCache = Cached {
    cacheFile  = resolvedComponentsCacheFile,
    cachedAction = \tcfs comps ma -> do
        Cradle {..} <- cradle
        let mums =
              case invalidatingInputFiles tcfs of
                Nothing -> Nothing
                Just iifs ->
                  let
                      filterOutSetupCfg =
                          filter (/= cradleRootDir </> setupConfigPath)
                      changedFiles = filterOutSetupCfg iifs
                  in if null changedFiles
                       then Nothing
                       else Just $ map Left changedFiles

        case ma of
          Just mcs -> gmsGet >>= \s -> gmsPut s { gmComponents = mcs }
          Nothing -> return ()

--        liftIO $ print ("changed files", mums :: Maybe [Either FilePath ()])

        mcs <- resolveGmComponents mums comps
        return (setupConfigPath:flatten mcs , mcs)
 }

 where
   flatten :: Map.Map ChComponentName (GmComponent t (Set.Set ModulePath))
           -> [FilePath]
   flatten = Map.elems
      >>> map (gmcHomeModuleGraph >>> gmgGraph
               >>> Map.elems
               >>> map (Set.map mpPath)
               >>> Set.unions
              )
      >>> Set.unions
      >>> Set.toList

moduleComponents :: Map ChComponentName (GmComponent t (Set ModulePath))
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


findCandidates :: [Set ChComponentName] -> Set ChComponentName
findCandidates [] = Set.empty
findCandidates scns = foldl1 Set.intersection scns

pickComponent :: Set ChComponentName -> ChComponentName
pickComponent scn = Set.findMin scn

packageGhcOptions :: (Applicative m, MonadIO m, GmEnv m, GmLog m) => m [GHCOption]
packageGhcOptions = do
    crdl <- cradle
    case cradleCabalFile crdl of
      Just _ -> getGhcMergedPkgOptions
      Nothing -> sandboxOpts crdl

sandboxOpts :: Monad m => Cradle -> m [String]
sandboxOpts crdl =
    return $ ["-i" ++ d | d <- [wdir,rdir]] ++ pkgOpts ++ ["-Wall"]
  where
    pkgOpts = ghcDbStackOpts $ cradlePkgDbStack crdl
    (wdir, rdir) = (cradleCurrentDir crdl, cradleRootDir crdl)

resolveGmComponent :: (IOish m, GmLog m, GmEnv m)
    => Maybe [CompilationUnit] -- ^ Updated modules
    -> GmComponent GMCRaw (Set ModulePath)
    -> m (GmComponent GMCResolved (Set ModulePath))
resolveGmComponent mums c@GmComponent {..} = do
  withLightHscEnv ghcOpts $ \env -> do
    let srcDirs = if null gmcSourceDirs then [""] else gmcSourceDirs
    let mg = gmcHomeModuleGraph
    let simp = gmcEntrypoints
    sump <- case mums of
        Nothing -> return simp
        Just ums ->
            Set.fromList . catMaybes <$>
               mapM (resolveModule env srcDirs) ums

    mg' <- canonicalizeModuleGraph =<< updateHomeModuleGraph env mg simp sump

    return $ c { gmcEntrypoints = simp, gmcHomeModuleGraph = mg' }

 where ghcOpts = concat [
           gmcGhcSrcOpts,
           gmcGhcLangOpts,
           [ "-optP-include", "-optP" ++ macrosHeaderPath ]
        ]

resolveEntrypoint :: (IOish m, GmLog m)
    => Cradle
    -> GmComponent GMCRaw ChEntrypoint
    -> m (GmComponent GMCRaw (Set ModulePath))
resolveEntrypoint Cradle {..} c@GmComponent {..} = do
    withLightHscEnv gmcGhcSrcOpts $ \env -> do
      let srcDirs = if null gmcSourceDirs then [""] else gmcSourceDirs
      eps <- liftIO $ resolveChEntrypoints cradleRootDir gmcEntrypoints
      rms <- resolveModule env srcDirs `mapM` eps
      return c { gmcEntrypoints = Set.fromList $ catMaybes rms }

resolveChEntrypoints :: FilePath -> ChEntrypoint -> IO [CompilationUnit]
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

resolveModule :: (MonadIO m, GmLog m) =>
  HscEnv -> [FilePath] -> CompilationUnit -> m (Maybe ModulePath)
resolveModule env _srcDirs (Right mn) =
    liftIO $ traverse canonicalizeModulePath =<< findModulePath env mn
resolveModule env srcDirs (Left fn') = do
    mfn <-  liftIO $ findFile' srcDirs fn'
    case mfn of
      Nothing -> return Nothing
      Just fn'' -> do
          fn <-  liftIO $ canonicalizePath fn''
          emn <-  liftIO $ fileModuleName env fn
          case emn of
              Left errs -> do
                gmLog GmWarning ("resolveModule " ++ show fn) $
                  empty $+$ (vcat $ map text errs)
                return Nothing -- TODO: should expose these errors otherwise
                               -- modules with preprocessor/parse errors are
                               -- going to be missing
              Right mmn -> return $ Just $
                  case mmn of
                    Nothing -> mkMainModulePath fn
                    Just mn -> ModulePath mn fn
 where
   -- needed for ghc 7.4
   findFile' dirs file =
       getFirst . mconcat <$> mapM (fmap First . mightExist . (</>file)) dirs

   -- fileModuleName fn (dir:dirs)
   --     | makeRelative dir fn /= fn

type CompilationUnit = Either FilePath ModuleName

resolveGmComponents :: (IOish m, GmState m, GmLog m, GmEnv m)
    => Maybe [CompilationUnit]
        -- ^ Updated modules
    -> [GmComponent GMCRaw (Set ModulePath)]
    -> m (Map ChComponentName (GmComponent GMCResolved (Set ModulePath)))
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
        => (forall t a. GmComponent t a -> b)
        -> GmComponent u c -> GmComponent v d -> Bool
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
                text "Target needs interpeter, switching to LinkInMemory/HscInterpreted. Perfectly normal if anything is using TemplateHaskell, QuasiQuotes or PatternSynonyms."
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
