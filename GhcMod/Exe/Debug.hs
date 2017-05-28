module GhcMod.Exe.Debug (debugInfo, rootInfo, componentInfo) where

import Control.Arrow (first)
import Control.Applicative
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char
import Data.Maybe
import Data.Version
import Data.List.Split
import System.Directory

import GhcMod.Exe.Internal
import GhcMod.Cradle
import GhcMod.Monad
import GhcMod.Output
import GhcMod.Pretty
import GhcMod.Stack
import GhcMod.Target
import GhcMod.Types
import GhcMod.Utils

import Paths_ghc_mod (version)

import Config (cProjectVersion)
import Pretty

----------------------------------------------------------------

-- | Obtaining debug information.
debugInfo :: IOish m => GhcModT m String
debugInfo = do
    Options {..} <- options
    Cradle {..} <- cradle

    [ghcPath, ghcPkgPath] <- liftIO $
        case cradleProject of
          StackProject se ->
              catMaybes <$> sequence [getStackGhcPath se, getStackGhcPkgPath se]
          _ ->
              return ["ghc", "ghc-pkg"]

    cabal <-
        case cradleProject of
          CabalProject -> cabalDebug ghcPkgPath
          StackProject {} -> (++) <$> stackPaths <*> cabalDebug ghcPkgPath
          _ -> return []

    pkgOpts <- packageGhcOptions

    readProc <- gmReadProcess

    ghcVersion <- liftIO $
        dropWhileEnd isSpace <$> readProc ghcPath ["--numeric-version"] ""

    return $ unlines $
      [ "Version:              ghc-mod-" ++ showVersion version
      , "Library GHC Version:  " ++ cProjectVersion
      , "System GHC Version:   " ++ ghcVersion
      , "Root directory:       " ++ cradleRootDir
      , "Current directory:    " ++ cradleCurrentDir
      , "GHC Package flags:\n"   ++ renderGm (nest 4 $
              fsep $ map text pkgOpts)
      , "GHC System libraries: " ++ ghcLibDir
      ] ++ cabal

stackPaths :: IOish m => GhcModT m [String]
stackPaths = do
    Cradle { cradleProject = StackProject senv } <- cradle
    ghc <- getStackGhcPath senv
    ghcPkg <- getStackGhcPkgPath senv
    return $
         [ "Stack ghc executable:    " ++ show ghc
         , "Stack ghc-pkg executable:" ++ show ghcPkg
         ]

cabalDebug :: IOish m => FilePath -> GhcModT m [String]
cabalDebug ghcPkgPath = do
    Cradle {..} <- cradle
    mcs <- cabalResolvedComponents
    let entrypoints = Map.map gmcEntrypoints mcs
        graphs      = Map.map gmcHomeModuleGraph mcs
        opts        = Map.map gmcGhcOpts mcs
        srcOpts     = Map.map gmcGhcSrcOpts mcs

    readProc <- gmReadProcess
    cabalExists <- liftIO $ (/=Nothing) <$> findExecutable "cabal"

    cabalInstVersion <-
        if cabalExists
          then liftIO $
            dropWhileEnd isSpace <$> readProc "cabal" ["--numeric-version"] ""
          else return ""

    packages <- liftIO $ readProc ghcPkgPath ["list", "--simple-output"] ""
    let cabalPackages = filter ((== ["Cabal"]) . take 1 . splitOn "-") $ splitWhen isSpace packages

    return $
         [ "cabal-install Version: "    ++ cabalInstVersion
         , "Cabal Library Versions:\n"  ++ renderGm (nest 4 $
              fsep $ map text cabalPackages)
         , "Cabal file:            "    ++ show cradleCabalFile
         , "Project:               " ++ show cradleProject
         , "Cabal entrypoints:\n"       ++ renderGm (nest 4 $
              mapDoc gmComponentNameDoc smpDoc entrypoints)
         , "Cabal components:\n"        ++ renderGm (nest 4 $
              mapDoc gmComponentNameDoc graphDoc graphs)
         , "GHC Cabal options:\n"       ++ renderGm (nest 4 $
              mapDoc gmComponentNameDoc (fsep . map text) opts)
         , "GHC search path options:\n" ++ renderGm (nest 4 $
              mapDoc gmComponentNameDoc (fsep . map text) srcOpts)
         ]

componentInfo :: IOish m => [String] -> GhcModT m String
componentInfo ts = do
    -- TODO: most of this is copypasta of targetGhcOptions. Factor out more
    -- useful function from there.
    crdl <- cradle
    sefnmn <- Set.fromList `liftM` mapM guessModuleFile ts
    mcs <- cabalResolvedComponents
    let
        mdlcs = moduleComponents mcs `zipMap` Set.toList sefnmn
        candidates = findCandidates $ map snd mdlcs
        cn = pickComponent candidates
    opts <- targetGhcOptions crdl sefnmn

    return $ unlines $
         [ "Matching Components:\n"     ++ renderGm (nest 4 $
              alistDoc (either text mnDoc) (setDoc gmComponentNameDoc) mdlcs)
         , "Picked Component:\n"        ++ renderGm (nest 4 $
              gmComponentNameDoc cn)
         , "GHC Cabal options:\n"       ++ renderGm (nest 4 $ fsep $ map text opts)
         ]
 where
   zipMap f l = l `zip` (f `map` l)

guessModuleFile :: MonadIO m => String -> m (Either FilePath ModuleName)
guessModuleFile m
  | (isUpper . head .&&. (all $ all $ isAlphaNum .||. (=='.')) . splitOn ".") m =
      return $ Right $ mkModuleName m
 where
   infixr 1 .||.
   infixr 2 .&&.
   (.||.) = liftA2 (||)
   (.&&.) = liftA2 (&&)

guessModuleFile str = Left `liftM` liftIO (canonFilePath str)

graphDoc :: GmModuleGraph -> Doc
graphDoc GmModuleGraph{..} =
    mapDoc mpDoc smpDoc' gmgGraph
 where
   smpDoc' smp = vcat $ map mpDoc' $ Set.toList smp
   mpDoc' = text . moduleNameString . mpModule

setDoc :: (a -> Doc) -> Set.Set a -> Doc
setDoc f s = vcat $ map f $ Set.toList s

smpDoc :: Set.Set ModulePath -> Doc
smpDoc smp = setDoc mpDoc smp

mpDoc :: ModulePath -> Doc
mpDoc (ModulePath mn fn) = text (moduleNameString mn) <+> parens (text fn)

mnDoc :: ModuleName -> Doc
mnDoc mn = text (moduleNameString mn)

alistDoc :: Ord k => (k -> Doc) -> (a -> Doc) -> [(k, a)] -> Doc
alistDoc fk fa alist = mapDoc fk fa (Map.fromList alist)

mapDoc :: (k -> Doc) -> (a -> Doc) -> Map.Map k a -> Doc
mapDoc kd ad m = vcat $
    map (uncurry ($+$)) $ map (first kd) $ Map.toList $ Map.map (nest 4 . ad) m
----------------------------------------------------------------

-- | Obtaining root information.
rootInfo :: forall m. (IOish m, GmOut m, GmEnv m) => m String
rootInfo = do
    crdl <- findCradleNoLog =<< (optPrograms <$> options)
    liftIO $ cleanupCradle crdl
    return $ cradleRootDir crdl ++ "\n"
