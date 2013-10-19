module Language.Haskell.GhcMod.Find (findSymbol, findSym) where

import Control.Monad (foldM, void)
import Control.Applicative ((<$>))
import Data.List
import Language.Haskell.GhcMod.List (listModsInternal)
import Language.Haskell.GhcMod.Browse (browseInternal)
import Language.Haskell.GhcMod.GHCApi
import Language.Haskell.GhcMod.Types
import GHC


-- | Return modules containing symbol.
findSymbol :: Options -> Cradle -> SymbolString -> IO String
findSymbol opts cradle symbol = 
   convert opts . nub . sort <$> withGHCDummyFile (findSym opts cradle symbol)

-- | Return modules containing symbol.
findSym :: Options -> Cradle -> SymbolString -> Ghc [String]
findSym opts cradle symbol = do
   void $ initializeFlagsWithCradle opts cradle [] False
   (modulesWith symbol) =<< listModsInternal
  where
   modulesWith sym = foldM (hasSym sym) []

   hasSym sym modsWithSym mod = do
      syms <- browseInternal mod typeInfo 
      return $ case find (== sym) syms of
                    Just _ -> mod : modsWithSym
                    _      -> modsWithSym

   typeInfo = detailed opts
