module Language.Haskell.GhcMod.Debug (debugInfo, rootInfo, componentInfo) where

import Control.Arrow (first)
import Control.Applicative ((<$>))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char
import Text.PrettyPrint
import Language.Haskell.GhcMod.Convert
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Internal
import Language.Haskell.GhcMod.CabalHelper
import Language.Haskell.GhcMod.Target
import Language.Haskell.GhcMod.Pretty

----------------------------------------------------------------

-- | Obtaining debug information.
debugInfo :: IOish m => GhcModT m String
debugInfo = do
    Options {..} <- options
    Cradle {..} <- cradle

    cabal <-
        case cradleCabalFile of
          Just _ -> cabalDebug
          Nothing -> return []

    return $ unlines $
      [ "Root directory:       " ++ cradleRootDir
      , "Current directory:    " ++ cradleCurrentDir
      , "GHC System libraries: " ++ ghcLibDir
      , "GHC user options:     " ++ render (fsep $ map text ghcUserOptions)
      ] ++ cabal

cabalDebug :: IOish m => GhcModT m [String]
cabalDebug = do
    crdl@Cradle {..} <- cradle
    mcs <- resolveGmComponents Nothing =<< mapM (resolveEntrypoint crdl) =<< getComponents
    let entrypoints = Map.map gmcEntrypoints mcs
        graphs      = Map.map gmcHomeModuleGraph mcs
        opts        = Map.map gmcGhcOpts mcs
        srcOpts     = Map.map gmcGhcSrcOpts mcs

    return $
         [ "Cabal file:           " ++ show cradleCabalFile
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
    crdl <- cradle
    opts <- targetGhcOptions crdl $ Set.fromList $ map guessModuleFile ts

    return $ unlines $
         [ "GHC Cabal options:\n"       ++ render (nest 4 $ fsep $ map text opts)
         ]

guessModuleFile :: String -> Either FilePath ModuleName
guessModuleFile mn@(h:r)
    | isUpper h && all isAlphaNum r = Right $ mkModuleName mn
guessModuleFile str = Left str

graphDoc :: GmModuleGraph -> Doc
graphDoc GmModuleGraph{..} =
    mapDoc mpDoc' smpDoc' gmgGraph
 where
   smpDoc' smp = vcat $ map mpDoc' $ Set.toList smp
   mpDoc' = text . moduleNameString . mpModule

smpDoc :: Set.Set ModulePath -> Doc
smpDoc smp = vcat $ map mpDoc $ Set.toList smp

mpDoc :: ModulePath -> Doc
mpDoc (ModulePath mn fn) = text (moduleNameString mn) <+> parens (text fn)


mapDoc :: (k -> Doc) -> (a -> Doc) -> Map.Map k a -> Doc
mapDoc kd ad m = vcat $
    map (uncurry ($+$)) $ map (first kd) $ Map.toList $ Map.map (nest 4 . ad) m
----------------------------------------------------------------

-- | Obtaining root information.
rootInfo :: IOish m => GhcModT m String
rootInfo = convert' =<< cradleRootDir <$> cradle
