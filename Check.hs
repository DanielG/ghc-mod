module Check (checkSyntax) where

import Cabal
import Control.Applicative
import CoreMonad
import GHC
import Prelude hiding (catch)
import Types

----------------------------------------------------------------

checkSyntax :: Options -> String -> IO String
checkSyntax opt file = unlines <$> check opt file

----------------------------------------------------------------

check :: Options -> String -> IO [String]
check opt fileName = withGHC $ do
    (file,readLog) <- initializeGHC opt fileName options True
    setTargetFile file
    load LoadAllTargets -- `gcatch` handleParseError ref xxx
    liftIO readLog
  where
    options = ["-Wall","-fno-warn-unused-do-bind"] ++ map ("-i" ++) (checkIncludes opt)
    {-
    handleParseError ref e = do
        liftIO . writeIORef ref $ errBagToStrList . srcErrorMessages $ e
        return Succeeded
    -}