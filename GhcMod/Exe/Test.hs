{-# LANGUAGE CPP #-}

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
#if __GLASGOW_HASKELL__ >= 804
    let Just ms = find ((==Just f') . ml_hs_file . ms_location) (mgModSummaries mg)
#else
    let Just ms = find ((==Just f') . ml_hs_file . ms_location) mg
#endif
        mdl = ms_mod ms
        mn = moduleName mdl

    mmi <- getModuleInfo mdl
    mi <- case mmi of
            Just mi -> return mi
            _ -> error "pattern match fail"
    let exs = map (occNameString . getOccName) $ modInfoExports mi
        cqs = filter ("prop_" `isPrefixOf`) exs

    setContext [ IIDecl $ simpleImportDecl mn
               , IIDecl $ simpleImportDecl $ mkModuleName "Test.QuickCheck"
               ]

    _res <- mapM runTest cqs

    return ""

#if __GLASGOW_HASKELL__ >= 802
runTest :: GhcMonad m => String -> m (Maybe SomeException)
runTest fn = do
  res <- execStmt ("quickCheck " ++ fn) execOptions
  return $ case res of
    ExecComplete (Right _) _ -> Nothing
    ExecComplete (Left se) _ -> Just se
    _                        -> error "runTest"
#else
runTest :: GhcMonad m => String -> m (Maybe SomeException)
runTest fn = do
  res <- runStmt ("quickCheck " ++ fn) RunToCompletion
  return $ case res of
    RunOk [] -> Nothing
    RunException se -> Just se
    _ -> error "runTest"
#endif
