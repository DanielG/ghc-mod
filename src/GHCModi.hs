{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Concurrent
import qualified Control.Exception as E (handle, SomeException(..))
import Control.Monad (when, void)
import Data.Function
import Data.List (intercalate, groupBy, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Exception as G (ghandle)
import GHC
import GhcMonad
import HscTypes (SourceError)
import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Internal
import System.IO (hFlush,stdout)

----------------------------------------------------------------

type DB = Map String [String]
type Logger = IO [String]

----------------------------------------------------------------

-- Running two GHC monad threads disables the handling of
-- C-c since installSignalHandlers is called twice, sigh.

main :: IO ()
main = E.handle handler $ do
    cradle <- findCradle
    mvar <- liftIO newEmptyMVar
    mlibdir <- getSystemLibDir
    void $ forkIO $ setupDB cradle mlibdir opt mvar
    run cradle mlibdir opt $ loop S.empty ls mvar
  where
    opt = defaultOptions
    ls = lineSeparator opt
    LineSeparator lsc = ls
    handler (E.SomeException e) = do
        putStr "ghc-modi:0:0:Error:"
        let x = intercalate lsc $ lines $ show e
        putStrLn x
        putStrLn "NG"

----------------------------------------------------------------

run :: Cradle -> Maybe FilePath -> Options -> (Logger -> Ghc a) -> IO a
run cradle mlibdir opt body = runGhc mlibdir $ do
    (readLog,_) <- initializeFlagsWithCradle opt cradle ["-Wall"] True
    dflags <- getSessionDynFlags
    defaultCleanupHandler dflags $ body readLog

----------------------------------------------------------------

setupDB :: Cradle -> Maybe FilePath -> Options -> MVar DB -> IO ()
setupDB cradle mlibdir opt mvar = do
    sm <- run cradle mlibdir opt $ \_ -> getSessionDynFlags >>= browseAll
    let sms = map tieup $ groupBy ((==) `on` fst) $ sort sm
        m = M.fromList sms
    putMVar mvar m
  where
    tieup x = (head (map fst x), map snd x)

----------------------------------------------------------------

loop :: Set FilePath -> LineSeparator -> MVar DB -> Logger -> Ghc ()
loop set ls mvar readLog  = do
    cmdArg <- liftIO $ getLine
    let (cmd,arg') = break (== ' ') cmdArg
        arg = dropWhile (== ' ') arg'
    (msgs,ok,set') <- case cmd of
        "check" -> checkStx set ls readLog arg
        "find"  -> findSym set mvar arg
        _       -> return ([], False, set)
    mapM_ (liftIO . putStrLn) msgs
    liftIO $ putStrLn $ if ok then "OK" else "NG"
    liftIO $ hFlush stdout
    when ok $ loop set' ls mvar readLog

----------------------------------------------------------------

checkStx :: Set FilePath
         -> LineSeparator
         -> Logger
         -> FilePath
         -> Ghc ([String], Bool, Set FilePath)
checkStx set ls readLog file = do
    let add = not $ S.member file set
    G.ghandle handler $ do
        when add $ addTargetFiles [file]
        void $ load LoadAllTargets
        msgs <- liftIO $ readLog
        let set' = if add then S.insert file set else set
        return (msgs, True, set')
  where
    handler :: SourceError -> Ghc ([String], Bool, Set FilePath)
    handler err = do
        errmsgs <- handleErrMsg ls err
        return (errmsgs, False, set)

findSym :: Set FilePath -> MVar DB -> String
        -> Ghc ([String], Bool, Set FilePath)
findSym set mvar sym = do
    db <- liftIO $ readMVar mvar
    let ret = case M.lookup sym db of
            Nothing -> []
            Just xs -> xs
    return (ret, True, set)
