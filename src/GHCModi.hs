module Main where

import System.IO
import Control.Monad
import CoreMonad (liftIO)
import Data.Set as S
import Exception (ghandle)
import GHC
import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Internal

main :: IO ()
main = do
    cradle <- findCradle
    void $ withGHCDummyFile $ do
        (readLog,_) <- initializeFlagsWithCradle opt cradle ["-Wall"] True
        loop readLog ls S.empty
        return []
    return ()
  where
    opt = defaultOptions
    ls = lineSeparator opt

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
