-- $ ghc -package ghc -package ghc-paths GhcTestcase.hs
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import GHC
import GHC.Paths (libdir)
import DynFlags

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $
      doStuff "Main.hs" "Main" args

doStuff :: String -> String -> [String] -> Ghc ()
doStuff targetFile targetModule args = do
    dflags0 <- getSessionDynFlags
    let dflags1 = dflags0 {
        ghcMode   = CompManager
      , ghcLink   = LinkInMemory
      , hscTarget = HscInterpreted
      , optLevel  = 0
      }
    (dflags2, _, _) <- parseDynamicFlags dflags1 (map noLoc args)
    _ <- setSessionDynFlags dflags2

    target <- guessTarget targetFile Nothing
    setTargets [target { targetAllowObjCode = True }]

    _ <- load LoadAllTargets

    setContext [IIModule $ mkModuleName targetModule]

    return ()
