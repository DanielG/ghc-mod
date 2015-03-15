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

module Language.Haskell.GhcMod.CabalHelper (
    getComponents
  , getGhcPkgOptions
  ) where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Distribution.Helper
import qualified Language.Haskell.GhcMod.Types as T
import Language.Haskell.GhcMod.Types hiding (ghcProgram, ghcPkgProgram,
                                             cabalProgram)
import Language.Haskell.GhcMod.Monad.Types
import Language.Haskell.GhcMod.Utils
import Language.Haskell.GhcMod.World
import Language.Haskell.GhcMod.PathsAndFiles
import System.FilePath

-- | Only package related GHC options, sufficient for things that don't need to
-- access home modules
getGhcPkgOptions :: (MonadIO m, GmEnv m) => m [(ChComponentName, [GHCOption])]
getGhcPkgOptions = do
  Cradle {..} <- cradle
  let distdir = cradleRootDir </> "dist"
  runQuery distdir ghcPkgOptions

helperProgs :: Options -> Programs
helperProgs opts = Programs {
                            cabalProgram  = T.cabalProgram opts,
                            ghcProgram    = T.ghcProgram opts,
                            ghcPkgProgram = T.ghcPkgProgram opts
                          }

-- | Primary interface to cabal-helper and intended single entrypoint to
-- constructing 'GmComponent's
--
-- The Component\'s 'gmcHomeModuleGraph' will be empty and has to be resolved by
-- 'resolveGmComponents'.
getComponents :: (MonadIO m, GmEnv m) => m [GmComponent ChEntrypoint]
getComponents = withCabal $ do
    Cradle {..} <- cradle
    let distdir = cradleRootDir </> "dist"
    opt <- options

    runQuery' (helperProgs opt) distdir $ do
      q <- liftM4 join4 ghcOptions ghcSrcOptions entrypoints sourceDirs
      return $ flip map q $ \(cn, (opts, (srcOpts, (ep, srcDirs)))) ->
          GmComponent cn opts srcOpts ep ep srcDirs mempty
 where
   join4 a b c = join' a . join' b . join' c
   join' :: Eq a => [(a,b)] -> [(a,c)] -> [(a,(b,c))]
   join' lb lc = [ (a, (b, c))
                | (a, b) <- lb
                , (a', c) <- lc
                , a == a'
                ]

withCabal :: (MonadIO m, GmEnv m) => m a -> m a
withCabal action = do
    crdl <- cradle
    opts <- options
    liftIO $ whenM (isSetupConfigOutOfDate <$> getCurrentWorld crdl) $
        withDirectory_ (cradleRootDir crdl) $ do
            let progOpts =
                    [ "--with-ghc=" ++ T.ghcProgram opts ]
                    -- Only pass ghc-pkg if it was actually set otherwise we
                    -- might break cabal's guessing logic
                    ++ if T.ghcPkgProgram opts /= T.ghcPkgProgram defaultOptions
                         then [ "--with-ghc-pkg=" ++ T.ghcPkgProgram opts ]
                         else []
            void $ readProcess (T.cabalProgram opts) ("configure":progOpts) ""
    action
