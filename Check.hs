module Check (checkSyntax) where

import Control.Applicative
import Control.Monad
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
        readLog <- initializeFlagsWithCradle opt cradle options True
        setTargetFile fileName
        slow <- needsTemplateHaskell <$> depanal [] False
        when slow setSlowDynFlags
        void $ load LoadAllTargets
        liftIO readLog
    options
      | expandSplice opt = "-w:"   : ghcOpts opt
      | otherwise        = "-Wall" : ghcOpts opt
