module Language.Haskell.GhcMod.Debug (debugInfo, rootInfo, componentInfo) where

import Control.Arrow (first)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Journal
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char
import Data.List.Split
import Text.PrettyPrint
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Internal
import Language.Haskell.GhcMod.Target
import Language.Haskell.GhcMod.Pretty
import Language.Haskell.GhcMod.Utils
import Language.Haskell.GhcMod.Cradle
import Language.Haskell.GhcMod.Stack

----------------------------------------------------------------

-- | Obtaining debug information.
debugInfo :: IOish m => GhcModT m String
debugInfo = do
    Options {..} <- options
    Cradle {..} <- cradle

    cabal <-
        case cradleProject of
          CabalProject -> cabalDebug
          StackProject {} -> (++) <$> stackPaths <*> cabalDebug
          _ -> return []

    pkgOpts <- packageGhcOptions

    return $ unlines $
      [ "Root directory:       " ++ cradleRootDir
      , "Current directory:    " ++ cradleCurrentDir
      , "GHC Package flags:\n"   ++ render (nest 4 $
              fsep $ map text pkgOpts)
      , "GHC System libraries: " ++ ghcLibDir
      , "GHC user options:\n"    ++ render (nest 4 $
              fsep $ map text optGhcUserOptions)
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

cabalDebug :: IOish m => GhcModT m [String]
cabalDebug = do
    Cradle {..} <- cradle
    mcs <- cabalResolvedComponents
    let entrypoints = Map.map gmcEntrypoints mcs
        graphs      = Map.map gmcHomeModuleGraph mcs
        opts        = Map.map gmcGhcOpts mcs
        srcOpts     = Map.map gmcGhcSrcOpts mcs

    return $
         [ "Cabal file:           " ++ show cradleCabalFile
         , "Project:   " ++ show cradleProject
         , "Cabal entrypoints:\n"       ++ render (nest 4 $
              mapDoc gmComponentNameDoc smpDoc entrypoints)
         , "Cabal components:\n"        ++ render (nest 4 $
              mapDoc gmComponentNameDoc graphDoc graphs)
         , "GHC Cabal options:\n"       ++ render (nest 4 $
              mapDoc gmComponentNameDoc (fsep . map text) opts)
         , "GHC search path options:\n" ++ render (nest 4 $
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
         [ "Matching Components:\n"     ++ render (nest 4 $
              alistDoc (either text mnDoc) (setDoc gmComponentNameDoc) mdlcs)
         , "Picked Component:\n"        ++ render (nest 4 $
              gmComponentNameDoc cn)
         , "GHC Cabal options:\n"       ++ render (nest 4 $ fsep $ map text opts)
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
rootInfo :: forall m. (IOish m, GmOut m) => m String
rootInfo = (++"\n") . cradleRootDir <$> fst `liftM` (runJournalT findCradle :: m (Cradle, GhcModLog))
