module GhcMod.Exe.Test where

import Control.Applicative
import Data.List
import System.FilePath
import System.Directory
import Prelude

import GhcMod.Types
import GhcMod.Monad
import GhcMod.DynFlags

import GHC
import GHC.Exception
import OccName

test :: IOish m
      => FilePath -> GhcModT m String
test f = runGmlT' [Left f] (fmap setHscInterpreted . deferErrors) $ do
    mg <- getModuleGraph
    root <- cradleRootDir <$> cradle
    f' <- makeRelative root <$> liftIO (canonicalizePath f)
    let Just ms = find ((==Just f') . ml_hs_file . ms_location) mg
        mdl = ms_mod ms
        mn = moduleName mdl

    Just mi <- getModuleInfo mdl
    let exs = map (occNameString . getOccName) $ modInfoExports mi
        cqs = filter ("prop_" `isPrefixOf`) exs

    setContext [ IIDecl $ simpleImportDecl mn
               , IIDecl $ simpleImportDecl $ mkModuleName "Test.QuickCheck"
               ]

    _res <- mapM runTest cqs

    return ""

runTest :: GhcMonad m => String -> m (Maybe SomeException)
runTest fn = do
  res <- runStmt ("quickCheck " ++ fn) RunToCompletion
  return $ case res of
    RunOk [] -> Nothing
    RunException se -> Just se
    _ -> error "runTest"
