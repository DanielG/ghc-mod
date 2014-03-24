module Main where

import Control.Monad (when, void)
import CoreMonad (liftIO)
import Data.List (intercalate)
import Data.Set as S
import Exception (ghandle, SomeException(..))
import GHC
import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Internal
import System.IO (hFlush,stdout)

main :: IO ()
main = do
    cradle <- findCradle
    run ls $ do
        (readLog,_) <- initializeFlagsWithCradle opt cradle ["-Wall"] True
        loop readLog ls S.empty
  where
    opt = defaultOptions
    ls = lineSeparator opt

run :: LineSeparator -> Ghc () -> IO ()
run (LineSeparator ls) body = do
    mlibdir <- getSystemLibDir
    ghandle ignore $ runGhc mlibdir $ do
        dflags <- getSessionDynFlags
        defaultCleanupHandler dflags body
  where
    ignore (SomeException e) = do
        putStr "ghc-modi:0:0:Error:"
        let x = intercalate ls $ lines $ show e
        putStrLn x
        putStrLn "NG"

loop :: IO [String] -> LineSeparator -> Set FilePath -> Ghc ()
loop readLog ls set = do
    cmdArg <- liftIO $ getLine
    let (cmd,arg') = break (== ' ') cmdArg
        arg = dropWhile (== ' ') arg'
    (msgs,ok,set') <- case cmd of
        "check" -> checkStx readLog ls set arg
        _       -> return ([], False, set)
    mapM_ (liftIO . putStrLn) msgs
    liftIO $ putStrLn $ if ok then "OK" else "NG"
    liftIO $ hFlush stdout
    when ok $ loop readLog ls set'

checkStx :: IO [String]
         -> LineSeparator
         -> Set FilePath
         -> FilePath
         -> Ghc ([String], Bool, Set FilePath)
checkStx readLog ls set file = do
    let add = not $ S.member file set
    ghandle handler $ do
        when add $ addTargetFiles [file]
        void $ load LoadAllTargets
        msgs <- liftIO $ readLog
        let set' = if add then S.insert file set else set
        return (msgs, True, set')
  where
    handler err = do
        errmsgs <- handleErrMsg ls err
        return (errmsgs, False, set)
