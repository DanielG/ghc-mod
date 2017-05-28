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

{-# LANGUAGE ExistentialQuantification #-}
module GhcMod.Error (
    GhcModError(..)
  , GmError
  , gmeDoc
  , ghcExceptionDoc
  , liftMaybe
  , overrideError
  , modifyError
  , modifyError'
  , modifyGmError
  , tryFix
  , GHandler(..)
  , gcatches
  , module Control.Monad.Error
  , module Control.Exception
  ) where

import Control.Arrow hiding ((<+>))
import Control.Exception
import Control.Monad.Error hiding (MonadIO, liftIO)
import qualified Data.Set as Set
import Data.List
import Data.Version
import System.Process (showCommandForUser)
import Text.Printf

import Exception
import Panic
import Pretty
import Config (cProjectVersion, cHostPlatformString)
import Paths_ghc_mod (version)

import GhcMod.Types
import GhcMod.Pretty

type GmError m = MonadError GhcModError m

gmeDoc :: GhcModError -> Doc
gmeDoc e = case e of
    GMENoMsg ->
        text "Unknown error"
    GMEString msg ->
        text msg
    GMECabalConfigure msg ->
        text "Configuring cabal project failed" <+>: gmeDoc msg
    GMEStackConfigure msg ->
        text "Configuring stack project failed" <+>: gmeDoc msg
    GMEStackBootstrap msg ->
        text "Bootstrapping stack project environment failed" <+>: gmeDoc msg
    GMECabalCompAssignment ctx ->
        text "Could not find a consistent component assignment for modules:" $$
          (nest 4 $ foldr ($+$) empty $ map ctxDoc ctx) $$
        text "" $$
        (if all (Set.null . snd) ctx
           then noComponentSuggestions
           else empty) $$
        text "- To find out which components ghc-mod knows about try:" $$
            nest 4 (backticks $ text "ghc-mod debug")

      where
        noComponentSuggestions =
          text "- Are some of these modules part of a test and or benchmark?\
               \ Try enabling them:" $$
              nest 4 (backticks $ text "cabal configure --enable-tests [--enable-benchmarks]")

        backticks d = char '`' <> d <> char '`'
        ctxDoc = moduleDoc *** compsDoc
                 >>> first (<> colon) >>> uncurry (flip hang 4)

        moduleDoc (Left fn)   =
            text "File " <> quotes (text fn)
        moduleDoc (Right mdl) =
            text "Module " <> quotes (text $ moduleNameString mdl)

        compsDoc sc | Set.null sc = text "has no known components"
        compsDoc sc = fsep $ punctuate comma $
                        map gmComponentNameDoc $ Set.toList sc
    GMEProcess _fn cmd args emsg -> let c = showCommandForUser cmd args in
        case emsg of
          Right err ->
             text (printf "Launching system command `%s` failed: " c)
                  <> gmeDoc err
          Left rv -> text $
             printf "Launching system command `%s` failed (exited with %d)" c rv
    GMENoCabalFile ->
        text "No cabal file found."
    GMETooManyCabalFiles cfs ->
        text $ "Multiple cabal files found. Possible cabal files: \""
               ++ intercalate "\", \"" cfs ++"\"."

ghcExceptionDoc :: GhcException -> Doc
ghcExceptionDoc e@(CmdLineError _) =
    text $ "<command line>: " ++ showGhcException e ""
ghcExceptionDoc (UsageError str) = strDoc str
ghcExceptionDoc (Panic msg) = vcat $ map text $ lines $ printf "\
\GHC panic! (the 'impossible' happened)\n\
\  ghc-mod version %s\n\
\  GHC library version %s for %s:\n\
\       %s\n\
\\n\
\Please report this as a bug: %s\n"
    gmVer ghcVer platform msg url
 where
   gmVer = showVersion version
   ghcVer = cProjectVersion
   platform = cHostPlatformString
   url = "https://github.com/kazu-yamamoto/ghc-mod/issues" :: String

ghcExceptionDoc e = text $ showGhcException e ""

liftMaybe :: MonadError e m => e -> m (Maybe a) -> m a
liftMaybe e action = maybe (throwError e) return =<< action

overrideError :: MonadError e m => e -> m a -> m a
overrideError e action = modifyError (const e) action

modifyError :: MonadError e m => (e -> e) -> m a -> m a
modifyError f action = action `catchError` \e -> throwError $ f e

infixr 0 `modifyError'`
modifyError' :: MonadError e m => m a -> (e -> e) -> m a
modifyError' = flip modifyError

modifyGmError :: (MonadIO m, ExceptionMonad m)
              => (GhcModError -> GhcModError) -> m a -> m a
modifyGmError f a = gcatch a $ \(ex :: GhcModError) -> liftIO $ throwIO (f ex)

tryFix :: MonadError e m => m a -> (e -> m ()) -> m a
tryFix action f = do
  action `catchError` \e -> f e >> action

data GHandler m a = forall e . Exception e => GHandler (e -> m a)

gcatches :: (MonadIO m, ExceptionMonad m) => m a -> [GHandler m a] -> m a
gcatches io handlers = io `gcatch` gcatchesHandler handlers

gcatchesHandler :: (MonadIO m, ExceptionMonad m)
    => [GHandler m a] -> SomeException -> m a
gcatchesHandler handlers e = foldr tryHandler (liftIO $ throw e) handlers
    where tryHandler (GHandler handler) res
              = case fromException e of
                Just e' -> handler e'
                Nothing -> res
