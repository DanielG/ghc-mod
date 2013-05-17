module Language.Haskell.GhcMod.Check (checkSyntax) where

import Control.Applicative
import Control.Monad
import CoreMonad
import Exception
import GHC
import Language.Haskell.GhcMod.ErrMsg
import Language.Haskell.GhcMod.GHCApi
import Language.Haskell.GhcMod.Types
import Prelude

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
        checkSlowAndSet
        void $ load LoadAllTargets
        liftIO readLog
    options
      | expandSplice opt = "-w:"   : ghcOpts opt
      | otherwise        = "-Wall" : ghcOpts opt
