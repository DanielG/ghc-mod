{-# LANGUAGE BangPatterns, CPP #-}

-- Commands:
--  check <file>
--  find <symbol>
--  lint <file> [hlint options]
--
-- Session separators:
--   OK -- success
--   NG -- failure

module Main where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, readMVar)
import Control.Exception (SomeException(..))
import qualified Control.Exception as E
import Control.Monad (when, void)
import Data.Function (on)
import Data.List (intercalate, groupBy, sort, find)
#if MIN_VERSION_containers(0,5,0)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
#else
import Data.Map (Map)
import qualified Data.Map as M
#endif
import Data.Set (Set)
import qualified Data.Set as S
import qualified Exception as GE
import GHC (Ghc, LoadHowMuch(LoadAllTargets), TargetId(TargetFile))
import qualified GHC as G
import GhcMonad (liftIO)
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
    handler (SomeException e) = do
        putStr "ghc-modi:0:0:"
        let x = intercalate lsc $ lines $ show e
        putStrLn x
        putStrLn "NG"

----------------------------------------------------------------

run :: Cradle -> Maybe FilePath -> Options -> (Logger -> Ghc a) -> IO a
run cradle mlibdir opt body = G.runGhc mlibdir $ do
    (readLog,_) <- initializeFlagsWithCradle opt cradle ["-Wall"] True
    dflags <- G.getSessionDynFlags
    G.defaultCleanupHandler dflags $ body readLog

----------------------------------------------------------------

setupDB :: Cradle -> Maybe FilePath -> Options -> MVar DB -> IO ()
setupDB cradle mlibdir opt mvar = E.handle handler $ do
    sm <- run cradle mlibdir opt $ \_ -> G.getSessionDynFlags >>= browseAll
    let sms = map tieup $ groupBy ((==) `on` fst) $ sort sm
        m = M.fromList sms
    putMVar mvar m
  where
    tieup x = (head (map fst x), map snd x)
    handler (SomeException _) = return ()

----------------------------------------------------------------

loop :: Set FilePath -> LineSeparator -> MVar DB -> Logger -> Ghc ()
loop set ls mvar readLog  = do
    cmdArg <- liftIO $ getLine
    let (cmd,arg') = break (== ' ') cmdArg
        arg = dropWhile (== ' ') arg'
    (msgs,ok,set') <- case cmd of
        "check" -> checkStx set ls readLog arg
        "find"  -> findSym set mvar arg
        "lint"  -> lintStx set ls arg
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
    GE.ghandle handler $ do
        mdel <- removeMainTarget
        when add $ addTargetFiles [file]
        void $ G.load LoadAllTargets
        msgs <- liftIO $ readLog
        let set1 = if add then S.insert file set else set
            set2 = case mdel of
                Nothing    -> set1
                Just delfl -> S.delete delfl set1
        return (msgs, True, set2)
  where
    handler :: SourceError -> Ghc ([String], Bool, Set FilePath)
    handler err = do
        errmsgs <- handleErrMsg ls err
        return (errmsgs, False, set)
    removeMainTarget = do
        mx <- find isMain <$> G.getModuleGraph
        case mx of
            Nothing -> return Nothing
            Just x  -> do
                let mainfile = G.ms_hspp_file x
                if mainfile == file then
                    return Nothing
                  else do
                    let target = TargetFile mainfile Nothing
                    G.removeTarget target
                    return $ Just mainfile
    isMain m = G.moduleNameString (G.moduleName (G.ms_mod m)) == "Main"

findSym :: Set FilePath -> MVar DB -> String
        -> Ghc ([String], Bool, Set FilePath)
findSym set mvar sym = do
    db <- liftIO $ readMVar mvar
    let ret = case M.lookup sym db of
            Nothing -> []
            Just xs -> xs
    return (ret, True, set)

lintStx :: Set FilePath -> LineSeparator -> FilePath
        -> Ghc ([String], Bool, Set FilePath)
lintStx set (LineSeparator lsep) fileOpts = liftIO $ do
    msgs <- map (intercalate lsep . lines) <$> lint hopts file
    return (msgs, True, set) -- fixme: error handling
  where
    file = fileOpts -- fixme
    hopts = [] -- fixme
