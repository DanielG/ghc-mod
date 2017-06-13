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

{-# LANGUAGE CPP, ViewPatterns, NamedFieldPuns, RankNTypes #-}
module GhcMod.Target where

import Control.Arrow
import Control.Applicative
import Control.Category ((.))
import GHC
#if __GLASGOW_HASKELL__ >= 800
import GHC.LanguageExtensions
#endif
import GHC.Paths (libdir)
import SysTools
import DynFlags
import HscTypes
import Pretty

import GhcMod.DynFlags
import GhcMod.Monad.Types
import GhcMod.CabalHelper
import GhcMod.HomeModuleGraph
import GhcMod.PathsAndFiles
import GhcMod.GhcPkg
import GhcMod.Error
import GhcMod.Logging
import GhcMod.Types
import GhcMod.Utils as U
import GhcMod.FileMapping
import GhcMod.LightGhc
import GhcMod.CustomPackageDb
import GhcMod.Output

import Safe
import Data.Maybe
import Data.Monoid as Monoid
import Data.Either
import Data.Foldable as Foldable (foldrM)
import qualified Data.Foldable as Foldable
import Data.Traversable hiding (mapM, forM)
import Data.IORef
import Data.List
import Data.Map (Map)
import qualified Data.Map  as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Function (on)
import Distribution.Helper
import Prelude hiding ((.))

import System.Directory
import System.FilePath

runGmPkgGhc :: (IOish m, Gm m) => LightGhc a -> m a
runGmPkgGhc action = do
    pkgOpts <- packageGhcOptions
    withLightHscEnv pkgOpts $ \env -> liftIO $ runLightGhc env action

initSession :: IOish m
            => [GHCOption]
            -> (forall gm. GhcMonad gm => DynFlags -> gm DynFlags)
            -> GhcModT m ()
initSession opts mdf = do
   s <- gmsGet
   case gmGhcSession s of
     Nothing -> do
         gmLog GmDebug "initSession" $ text "Session not initialized, creating new one"
         putNewSession s
     Just (GmGhcSession hsc_env_ref) -> do
         crdl <- cradle

         df <- liftIO $ hsc_dflags <$> readIORef hsc_env_ref
         changed <-
             withLightHscEnv' (initDF crdl) $ \hsc_env ->
               return $ not $ hsc_dflags hsc_env `eqDynFlags` df

         if changed
            then do
              gmLog GmDebug "initSession" $ text "Flags changed, creating new session"
              teardownSession hsc_env_ref
              putNewSession s
            else
              gmLog GmDebug "initSession" $ text "Session already initialized"
 where
   initDF Cradle { cradleTempDir } df =
       setTmpDir cradleTempDir <$> (mdf =<< addCmdOpts opts df)

   teardownSession hsc_env_ref = do
     hsc_env <- liftIO $ readIORef hsc_env_ref
     teardownLightEnv hsc_env

   putNewSession :: IOish m => GhcModState -> GhcModT m ()
   putNewSession s = do
     crdl <- cradle
     nhsc_env_ref <- liftIO . newIORef =<< newLightEnv (initDF crdl)
     runLightGhc' nhsc_env_ref $ setSessionDynFlags =<< getSessionDynFlags
     gmsPut s { gmGhcSession = Just $ GmGhcSession nhsc_env_ref }


-- | Drop the currently active GHC session, the next that requires a GHC session
-- will initialize a new one.
dropSession :: IOish m => GhcModT m ()
dropSession = do
  s <- gmsGet
  case gmGhcSession s of
    Just (GmGhcSession ref) -> do
      -- TODO: This is still not enough, there seem to still be references to
      -- GHC's state around afterwards.
      liftIO $ writeIORef ref (error "HscEnv: session was dropped")
      -- Not available on ghc<7.8; didn't really help anyways
      -- liftIO $ setUnsafeGlobalDynFlags (error "DynFlags: session was dropped")
      gmsPut s { gmGhcSession = Nothing }

    Nothing -> return ()

-- | Run a GmlT action (i.e. a function in the GhcMonad) in the context
-- of certain files or modules
runGmlT :: IOish m => [Either FilePath ModuleName] -> GmlT m a -> GhcModT m a
runGmlT fns action = runGmlT' fns return action

-- | Run a GmlT action (i.e. a function in the GhcMonad) in the context
-- of certain files or modules, with updated GHC flags
runGmlT' :: IOish m
              => [Either FilePath ModuleName]
              -> (forall gm. GhcMonad gm => DynFlags -> gm DynFlags)
              -> GmlT m a
              -> GhcModT m a
runGmlT' fns mdf action = runGmlTWith fns mdf id action

-- | Run a GmlT action (i.e. a function in the GhcMonad) in the context
-- of certain files or modules, with updated GHC flags and a final
-- transformation
runGmlTWith :: IOish m
                 => [Either FilePath ModuleName]
                 -> (forall gm. GhcMonad gm => DynFlags -> gm DynFlags)
                 -> (GmlT m a -> GmlT m b)
                 -> GmlT m a
                 -> GhcModT m b
runGmlTWith efnmns' mdf wrapper action = do
    crdl <- cradle
    Options { optGhcUserOptions } <- options

    let (fns, mns) = partitionEithers efnmns'
        ccfns = map (cradleCurrentDir crdl </>) fns
    cfns <- mapM getCanonicalFileNameSafe ccfns
    let serfnmn = Set.fromList $ map Right mns ++ map Left cfns
    opts <- targetGhcOptions crdl serfnmn
    let opts' = opts ++ ["-O0"] ++ optGhcUserOptions

    gmVomit
      "session-ghc-options"
      (text "Initializing GHC session with following options")
      (intercalate " " $ map (("\""++) . (++"\"")) opts')

    GhcModLog { gmLogLevel = Just level } <- gmlHistory
    putErr <- gmErrStrIO
    let setLogger | level >= GmDebug = setDebugLogger putErr
                  | otherwise = setEmptyLogger

    initSession opts' $
        setHscNothing >>> setLogger >>> mdf

    mappedStrs <- getMMappedFilePaths
    let targetStrs = mappedStrs ++ map moduleNameString mns ++ cfns

    unGmlT $ wrapper $ do
      loadTargets opts targetStrs
      action

targetGhcOptions :: forall m. IOish m
                  => Cradle
                  -> Set (Either FilePath ModuleName)
                  -> GhcModT m [GHCOption]
targetGhcOptions crdl sefnmn = do
    when (Set.null sefnmn) $ error "targetGhcOptions: no targets given"

    case cradleProject crdl of
      proj
          | isCabalHelperProject proj -> cabalOpts crdl
          | otherwise -> sandboxOpts crdl
 where
   zipMap f l = l `zip` (f `map` l)

   cabalOpts :: Cradle -> GhcModT m [String]
   cabalOpts Cradle{..} = do
       mcs <- cabalResolvedComponents

       let mdlcs = moduleComponents mcs `zipMap` Set.toList sefnmn
           candidates = findCandidates $ map snd mdlcs

       let noCandidates = Set.null candidates
           noModuleHasAnyAssignment = all (Set.null . snd) mdlcs

       if noCandidates && noModuleHasAnyAssignment
          then do
            -- First component should be ChLibName, if no lib will take lexically first exe.
            let cns = filter (/= ChSetupHsName) $ Map.keys mcs

            gmLog GmDebug "" $ strDoc $ "Could not find a component assignment, falling back to picking library component in cabal file."
            return $ gmcGhcOpts $ fromJustNote "targetGhcOptions, no-assignment" $ Map.lookup (head cns) mcs
          else do
            when noCandidates $
              throwError $ GMECabalCompAssignment mdlcs

            let cn = pickComponent candidates
            return $ gmcGhcOpts $ fromJustNote "targetGhcOptions" $ Map.lookup cn mcs

resolvedComponentsCache :: IOish m => FilePath ->
    Cached (GhcModT m) GhcModState
    [GmComponent 'GMCRaw (Set.Set ModulePath)]
    (Map.Map ChComponentName (GmComponent 'GMCResolved (Set.Set ModulePath)))
resolvedComponentsCache distdir = Cached {
    cacheLens = Just (lGmcResolvedComponents . lGmCaches),
    cacheFile = resolvedComponentsCacheFile distdir,
    cachedAction = \tcfs comps ma -> do
        Cradle {..} <- cradle
        let iifs = invalidatingInputFiles tcfs

            setupChanged =
                (cradleRootDir </> setupConfigPath distdir) `elem` iifs

            mums :: Maybe [Either FilePath ModuleName]
            mums =
              let
                  filterOutSetupCfg =
                      filter (/= cradleRootDir </> setupConfigPath distdir)
                  changedFiles = filterOutSetupCfg iifs
              in if null changedFiles || setupChanged
                   then Nothing
                   else Just $ map Left changedFiles

        let mdesc (Left f) = "file:" ++ f
            mdesc (Right mn) = "module:" ++ moduleNameString mn

            changed = map (text . mdesc) $ Foldable.concat mums
            changedDoc | [] <- changed = text "none"
                       | otherwise = sep changed

        gmLog GmDebug "resolvedComponentsCache" $
              text "files changed" <+>: changedDoc

        mcs <- resolveGmComponents ((,) <$> mums <*> ma) comps

        return (setupConfigPath distdir : flatten mcs , mcs)
 }

 where
   flatten :: Map.Map ChComponentName (GmComponent t (Set.Set ModulePath))
           -> [FilePath]
   flatten = Map.elems
      >>> map (gmcHomeModuleGraph >>> gmgGraph
               >>> (Map.keysSet &&& Map.elems)
               >>> uncurry insert
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

packageGhcOptions :: (IOish m, Applicative m, Gm m) => m [GHCOption]
packageGhcOptions = do
    crdl <- cradle
    case cradleProject crdl of
      proj
          | isCabalHelperProject proj -> getGhcMergedPkgOptions
          | otherwise -> sandboxOpts crdl

-- also works for plain projects!
sandboxOpts :: (IOish m, GmEnv m) => Cradle -> m [String]
sandboxOpts crdl = do
    mCusPkgDb <- getCustomPkgDbStack
    pkgDbStack <- liftIO $ getSandboxPackageDbStack
    let pkgOpts = ghcDbStackOpts $ fromMaybe pkgDbStack mCusPkgDb
    return $ ["-i" ++ d | d <- [wdir,rdir]] ++ pkgOpts ++ ["-Wall"]
  where
    (wdir, rdir) = (cradleCurrentDir crdl, cradleRootDir crdl)

    getSandboxPackageDbStack :: IO [GhcPkgDb]
    getSandboxPackageDbStack =
        ([GlobalDb] ++) . maybe [UserDb] return <$> getSandboxDb crdl

resolveGmComponent :: (IOish m, Gm m)
    => Maybe [CompilationUnit] -- ^ Updated modules
    -> GmComponent 'GMCRaw (Set ModulePath)
    -> m (GmComponent 'GMCResolved (Set ModulePath))
resolveGmComponent mums c@GmComponent {..} = do
  distDir <- cradleDistDir <$> cradle
  gmLog GmDebug "resolveGmComponent" $ text $ show $ ghcOpts distDir
  withLightHscEnv (ghcOpts distDir) $ \env -> do
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

 where ghcOpts distDir = concat [
           gmcGhcSrcOpts,
           gmcGhcLangOpts,
           [ "-optP-include", "-optP" ++ distDir </> macrosHeaderPath ]
        ]

resolveEntrypoint :: (IOish m, Gm m)
    => Cradle
    -> GmComponent 'GMCRaw ChEntrypoint
    -> m (GmComponent 'GMCRaw (Set ModulePath))
resolveEntrypoint Cradle {..} c@GmComponent {..} = do
    gmLog GmDebug "resolveEntrypoint" $ text $ show $ gmcGhcSrcOpts
    withLightHscEnv gmcGhcSrcOpts $ \env -> do
      let srcDirs = if null gmcSourceDirs then [""] else gmcSourceDirs
      eps <- liftIO $ resolveChEntrypoints cradleRootDir gmcEntrypoints
      rms <- resolveModule env srcDirs `mapM` eps
      return c { gmcEntrypoints = Set.fromList $ catMaybes rms }

-- TODO: remember that the file from `main-is:` is always module `Main` and let
-- ghc do the warning about it. Right now we run that module through
-- resolveModule like any other
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


resolveModule :: (IOish m, Gm m) =>
  HscEnv -> [FilePath] -> CompilationUnit -> m (Maybe ModulePath)
resolveModule env _srcDirs (Right mn) =
    liftIO $ traverse canonicalizeModulePath =<< findModulePath env mn
resolveModule env srcDirs (Left fn') = do
    mfn <-  liftIO $ findFile' srcDirs fn'
    case mfn of
      Nothing -> return Nothing
      Just fn'' -> do
          fn <-  liftIO $ canonicalizePath fn''
          emn <- fileModuleName env fn
          case emn of
              Left errs -> do
                gmLog GmWarning ("resolveModule " ++ show fn) $
                  Pretty.empty $+$ (vcat $ map text errs)
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
type Components =
    [GmComponent 'GMCRaw (Set ModulePath)]
type ResolvedComponentsMap =
    Map ChComponentName (GmComponent 'GMCResolved (Set ModulePath))

resolveGmComponents :: (IOish m, Gm m)
    => Maybe ([CompilationUnit], ResolvedComponentsMap)
        -- ^ Updated modules
    -> Components -> m ResolvedComponentsMap
resolveGmComponents mcache cs = do
    let rcm = fromMaybe Map.empty $ snd <$> mcache

    m' <- foldrM' rcm cs $ \c m -> do
        case Map.lookup (gmcName c) m of
          Nothing -> insertUpdated m c
          Just c' -> if same gmcRawEntrypoints c c' && same gmcGhcSrcOpts c c'
                       then return m
                       else insertUpdated m c
    return m'

 where
   foldrM' b fa f = foldrM f b fa
   insertUpdated m c = do
     rc <- resolveGmComponent (fst <$> mcache) c
     return $ Map.insert (gmcName rc) rc m

   same :: Eq b
        => (forall t a. GmComponent t a -> b)
        -> GmComponent u c -> GmComponent v d -> Bool
   same f a b = (f a) == (f b)

-- | Set the files as targets and load them.
loadTargets :: IOish m => [GHCOption] -> [FilePath] -> GmlT m ()
loadTargets opts targetStrs = do
    targets' <-
        withLightHscEnv opts $ \env ->
                liftM (nubBy ((==) `on` targetId))
                  (mapM ((`guessTarget` Nothing) >=> mapFile env) targetStrs)
              >>= mapM relativize

    let targets = map (\t -> t { targetAllowObjCode = False }) targets'

    gmLog GmDebug "loadTargets" $
          text "Loading" <+>: fsep (map (text . showTargetId) targets)

    setTargets targets

    mg <- depanal [] False

    let interp = needsHscInterpreted mg
    target <- hscTarget <$> getSessionDynFlags
    when (interp && target /= HscInterpreted) $ do
      _ <- setSessionDynFlags . setHscInterpreted =<< getSessionDynFlags
      gmLog GmInfo "loadTargets" $ text "Target needs interpeter, switching to LinkInMemory/HscInterpreted. Perfectly normal if anything is using TemplateHaskell, QuasiQuotes or PatternSynonyms."

    target' <- hscTarget <$> getSessionDynFlags

    case target' of
      HscNothing -> do
        void $ load LoadAllTargets
        forM_ mg $
          handleSourceError (gmLog GmDebug "loadTargets" . text . show)
          . void . (parseModule >=> typecheckModule >=> desugarModule)
      HscInterpreted -> do
        void $ load LoadAllTargets
      _ -> error ("loadTargets: unsupported hscTarget")

    gmLog GmDebug "loadTargets" $ text "Loading done"

  where
    relativize (Target (TargetFile filePath phase) taoc src) = do
      crdl <- cradle
      let tid = TargetFile relativeFilePath phase
          relativeFilePath = makeRelative (cradleRootDir crdl) filePath
      return $ Target tid taoc src
    relativize tgt = return tgt

    showTargetId (Target (TargetModule s) _ _) = moduleNameString s
    showTargetId (Target (TargetFile s _) _ _) = s

needsHscInterpreted :: ModuleGraph -> Bool
needsHscInterpreted = any $ \ms ->
                let df = ms_hspp_opts ms in
#if __GLASGOW_HASKELL__ >= 800
                   TemplateHaskell `xopt` df
                || QuasiQuotes     `xopt` df
                || PatternSynonyms `xopt` df
#else
                   Opt_TemplateHaskell `xopt` df
                || Opt_QuasiQuotes     `xopt` df
#if __GLASGOW_HASKELL__ >= 708
                || (Opt_PatternSynonyms `xopt` df)
#endif
#endif

cabalResolvedComponents :: (IOish m) =>
   GhcModT m (Map ChComponentName (GmComponent 'GMCResolved (Set ModulePath)))
cabalResolvedComponents = do
    crdl@(Cradle{..}) <- cradle
    comps <- mapM (resolveEntrypoint crdl) =<< getComponents
    withAutogen $
      cached cradleRootDir (resolvedComponentsCache cradleDistDir) comps
