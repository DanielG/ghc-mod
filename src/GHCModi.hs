module Main where

import Control.Monad
import CoreMonad (liftIO)
import Data.List
import Data.Set as S
import Exception (ghandle, SomeException(..))
import GHC
import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Internal
import System.IO

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
    file <- liftIO $ getLine
    let add = not $ S.member file set
    (errmsgs,ok) <- ghandle handler $ do
        when add $ addTargetFiles [file]
        void $ load LoadAllTargets
        msgs <- liftIO $ readLog
        return (msgs, True)
    mapM_ (liftIO . putStrLn) errmsgs
    liftIO $ putStrLn $ if ok then "OK" else "NG"
    liftIO $ hFlush stdout
    let set'
          | add && ok = S.insert file set
          | otherwise = set
    when ok $ loop readLog ls set'
  where
    handler err = do
        errmsgs <- handleErrMsg ls err
        return (errmsgs, False)
