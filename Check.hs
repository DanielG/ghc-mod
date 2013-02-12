module Check (checkSyntax) where

import Cabal
import Control.Applicative
import CoreMonad
import ErrMsg
import Exception
import GHC
import GHCApi
import Prelude
import Types

----------------------------------------------------------------

checkSyntax :: Options -> String -> IO String
checkSyntax opt file = unlines <$> check opt file

----------------------------------------------------------------

check :: Options -> String -> IO [String]
check opt fileName = withGHC' fileName $ checkIt `gcatch` handleErrMsg
  where
    checkIt = do
        (file,readLog) <- initializeGHC opt fileName options True
        setTargetFile file
        _ <- load LoadAllTargets
        liftIO readLog
    options
      | expandSplice opt = "-w:"   : ghcOpts opt
      | otherwise        = "-Wall" : ghcOpts opt
