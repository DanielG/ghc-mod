-- Copyright (C) 2013-2016  Carlo Hamalainen <carlo ÄT carlo-hamalainen DOT net>
-- Copyright (C) 2016  Daniel Gröber <dxld ÄT darkboxed DOT org>
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

-- {-# OPTIONS_GHC -fwarn-typed-holes -fdefer-type-errors #-}
module Language.Haskell.GhcMod.ImportedFrom {-# WARNING "TODO: remove use of strDoc" #-} (importedFrom) where

import Control.Applicative
import Control.Exception
import Data.List
import Data.Maybe
import System.FilePath

import Exception (ghandle)
import FastString
import GHC
import OccName
import Packages
import Name

import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.DynFlags
import Language.Haskell.GhcMod.FileMapping
import Language.Haskell.GhcMod.Gap
import Language.Haskell.GhcMod.Logging
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.SrcUtils (listifyStaged, findSpanName, cmp)
import GHC.SYB.Utils
import Data.Function
import Data.Version
import Prelude

-- | Look up Haddock docs for a symbol.
importedFrom
    :: forall m. IOish m
    => FilePath     -- ^ A target file.
    -> Int          -- ^ Line number.
    -> Int          -- ^ Column number.
    -> Expression   -- ^ Expression (symbol)
    -> GhcModT m String
importedFrom file lineNr colNr (Expression symbol) =
  ghandle handler $
    runGmlT' [Left file] deferErrors $
      withInteractiveContext $ do
        crdl         <- cradle
        modSum       <- fileModSummaryWithMapping (cradleCurrentDir crdl </> file)
        dflag        <- getSessionDynFlags
        (Just (decls,imports, _exports, _docs)) <- renamedSource <$> (parseModule modSum >>= typecheckModule)
        let
          ids :: [(SrcSpan, [Name.Name])]
          ids = sortBy (cmp `on` fst) $ findSpanName decls (lineNr, colNr)
          getVisibleExports (Just (hide, lie)) mi
            | hide = modInfoExports mi \\ list
            | otherwise = list
            where
              list = listifyStaged Renamer (const True) lie :: [Name.Name]
          getVisibleExports Nothing mi = modInfoExports mi
        mods <- mapM ((\ImportDecl{..} -> flip (,) ideclHiding <$> findModule (unLoc ideclName) ideclPkgQual) . unLoc) imports
        mis <- mapM (\(x, h) -> ((,) (moduleNameString $ moduleName x) . getVisibleExports h . fromJust) <$> getModuleInfo x) mods
        let
          outp = nub $ concatMap (mapMaybe f . snd) ids
          f :: Name.Name -> Maybe String
          f i
            | Just modul <- mmodul
            , symbol == occn
            = let
                unitid = modulePackageKey modul
                modulname = moduleNameString $ moduleName modul
                Just pkg = lookupPackage dflag unitid
                haddock = haddockHTMLs pkg
                PackageName pkgname' = packageName pkg
                pkgname = unpackFS pkgname'
                pkgver = showVersion $ packageVersion pkg
              in Just . unwords $
                [pkgname ++ "-" ++ pkgver ++ ":" ++ modulname ++ "." ++ occn, intercalate "," impmodul]
                ++ concatMap (\x -> map (\y -> x ++ '/' : dotToDash y ++ ".html") impmodul) haddock
            | otherwise = Nothing
            where
              name = getName i
              mmodul = nameModule_maybe name
              occn = occNameString $ getOccName i
              impmodul = map fst $ filter ((name `elem`) . snd) mis
              dotToDash =
                let
                  dtd '.' = '-'
                  dtd x = x
                in map dtd

        return $ unlines outp
 where
   handler (SomeException ex) = do
     gmLog GmException "types" $ showDoc ex
     return []
