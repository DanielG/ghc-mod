module Check (checkSyntax) where

import Control.Applicative
import CoreMonad
import ErrMsg
import Exception
import GHC
import GHCApi
import Prelude
import Types

----------------------------------------------------------------

checkSyntax :: Options -> Cradle -> String -> IO String
checkSyntax opt cradle file = unlines <$> check opt cradle file

----------------------------------------------------------------

check :: Options -> Cradle -> String -> IO [String]
check opt cradle fileName = withGHC fileName $ checkIt `gcatch` handleErrMsg
  where
    checkIt = do
        readLog <- initializeFlagsWithCradle opt cradle fileName options True
        setTargetFile fileName
        _ <- load LoadAllTargets
        liftIO readLog
    options
      | expandSplice opt = "-w:"   : ghcOpts opt
      | otherwise        = "-Wall" : ghcOpts opt
