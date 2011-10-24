module Check (checkSyntax) where

import Cabal
import Control.Applicative
import CoreMonad
import ErrMsg
import Exception
import GHC
import Prelude hiding (catch)
import Types

import CabalDev (modify_options)

----------------------------------------------------------------

checkSyntax :: Options -> String -> IO String
checkSyntax opt file = do
  opt' <- modify_options opt
  unlines <$> check opt' file

----------------------------------------------------------------

check :: Options -> String -> IO [String]
check opt fileName = withGHC $ checkIt `gcatch` handleErrMsg
  where
    checkIt = do
        (file,readLog) <- initializeGHC opt fileName options True
        setTargetFile file
        load LoadAllTargets
        liftIO readLog
    options = ["-Wall","-fno-warn-unused-do-bind"] ++ ghcOpts opt
           ++ map ("-i" ++) (checkIncludes opt)
