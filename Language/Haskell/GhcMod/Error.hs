{-# LANGUAGE TypeFamilies, ScopedTypeVariables, DeriveDataTypeable #-}
module Language.Haskell.GhcMod.Error (
    GhcModError(..)
  , GMConfigStateFileError(..)
  , GmError
  , gmeDoc
  , modifyError
  , modifyError'
  , tryFix
  , module Control.Monad.Error
  , module Exception
  ) where

import Control.Monad.Error (MonadError(..), Error(..))
import Data.List
import Data.Typeable
import Exception
import Text.PrettyPrint

type GmError m = MonadError GhcModError m

data GhcModError = GMENoMsg
                 -- ^ Unknown error

                 | GMEString String
                 -- ^ Some Error with a message. These are produced mostly by
                 -- 'fail' calls on GhcModT.

                 | GMEIOException IOException
                 -- ^ IOExceptions captured by GhcModT's MonadIO instance

                 | GMECabalConfigure GhcModError
                 -- ^ Configuring a cabal project failed.

                 | GMECabalFlags GhcModError
                 -- ^ Retrieval of the cabal configuration flags failed.

                 | GMEProcess [String] GhcModError
                 -- ^ Launching an operating system process failed. The first
                 -- field is the command.

                 | GMENoCabalFile
                 -- ^ No cabal file found.

                 | GMETooManyCabalFiles [FilePath]
                 -- ^ Too many cabal files found.

                 | GMECabalStateFile GMConfigStateFileError
                 -- ^ Reading Cabal's state configuration file falied somehow.
                   deriving (Eq,Show,Typeable)

data GMConfigStateFileError
    = GMConfigStateFileNoHeader
    | GMConfigStateFileBadHeader
    | GMConfigStateFileNoParse
    | GMConfigStateFileMissing
--    | GMConfigStateFileBadVersion PackageIdentifier PackageIdentifier (Either ConfigStateFileError LocalBuildInfo)
  deriving (Eq, Show, Read, Typeable)

gmCsfeDoc :: GMConfigStateFileError -> Doc
gmCsfeDoc GMConfigStateFileNoHeader = text $
        "Saved package config file header is missing. "
        ++ "Try re-running the 'configure' command."

gmCsfeDoc GMConfigStateFileBadHeader = text $
        "Saved package config file header is corrupt. "
        ++ "Try re-running the 'configure' command."

gmCsfeDoc GMConfigStateFileNoParse = text $
        "Saved package config file body is corrupt. "
        ++ "Try re-running the 'configure' command."

gmCsfeDoc GMConfigStateFileMissing = text $
    "Run the 'configure' command first."

-- gmCsfeDoc (ConfigStateFileBadVersion oldCabal oldCompiler _) = text $
--         "You need to re-run the 'configure' command. "
--         ++ "The version of Cabal being used has changed (was "
--         ++ display oldCabal ++ ", now "
--         ++ display currentCabalId ++ ")."
--         ++ badCompiler
--       where
--         badCompiler
--           | oldCompiler == currentCompilerId = ""
--           | otherwise =
--               " Additionally the compiler is different (was "
--               ++ display oldCompiler ++ ", now "
--               ++ display currentCompilerId
--               ++ ") which is probably the cause of the problem."





instance Exception GhcModError

instance Error GhcModError where
    noMsg = GMENoMsg
    strMsg = GMEString

gmeDoc :: GhcModError -> Doc
gmeDoc e = case e of
    GMENoMsg ->
        text "Unknown error"
    GMEString msg ->
        text msg
    GMEIOException ioe ->
        text $ show ioe
    GMECabalConfigure msg ->
        text "cabal configure failed: " <> gmeDoc msg
    GMECabalFlags msg ->
        text "retrieval of the cabal configuration flags failed: " <> gmeDoc msg
    GMEProcess cmd msg ->
        text ("launching operating system process `"++unwords cmd++"` failed: ")
          <> gmeDoc msg
    GMENoCabalFile ->
        text "No cabal file found."
    GMETooManyCabalFiles cfs ->
        text $ "Multiple cabal files found. Possible cabal files: \""
               ++ intercalate "\", \"" cfs ++"\"."
    GMECabalStateFile csfe ->
        gmCsfeDoc csfe


modifyError :: MonadError e m => (e -> e) -> m a -> m a
modifyError f action = action `catchError` \e -> throwError $ f e

infixr 0 `modifyError'`
modifyError' :: MonadError e m => m a -> (e -> e) -> m a
modifyError' = flip modifyError

tryFix :: MonadError e m => m a -> (e -> m ()) -> m a
tryFix action fix = do
  action `catchError` \e -> fix e >> action
