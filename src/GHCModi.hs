{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- Commands:
--  check <file>
--  find <symbol>
--  info <file> <expr>
--  type <file> <line> <column>
--  lint [hlint options] <file>
--     the format of hlint options is [String] because they may contain
--     spaces and aslo <file> may contain spaces.
--
-- Session separators:
--   OK -- success
--   NG -- failure

module Main where

#ifndef MIN_VERSION_containers
#define MIN_VERSION_containers(x,y,z) 1
#endif

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, readMVar)
import Control.Exception (SomeException(..))
import qualified Control.Exception as E
import Control.Monad (when, void)
import CoreMonad (liftIO)
import Data.Function (on)
import Data.List (intercalate, groupBy, sort, find)
#if MIN_VERSION_containers(0,5,0)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
#else
import Data.Map (Map)
import qualified Data.Map as M
#endif
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Typeable (Typeable)
import Data.Version (showVersion)
import qualified Exception as GE
import GHC (Ghc, LoadHowMuch(LoadAllTargets), TargetId(TargetFile))
import qualified GHC as G
import HscTypes (SourceError)
import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Ghc
import Language.Haskell.GhcMod.Internal
import Paths_ghc_mod
import System.Console.GetOpt
import System.Directory (setCurrentDirectory)
import System.Environment (getArgs)
import System.IO (hFlush,stdout)

----------------------------------------------------------------

type DB = Map String [String]
type Logger = IO String

----------------------------------------------------------------

argspec :: [OptDescr (Options -> Options)]
argspec = [ Option "b" ["boundary"]
            (ReqArg (\s opts -> opts { lineSeparator = LineSeparator s }) "sep")
            "specify line separator (default is Nul string)"
          , Option "l" ["tolisp"]
            (NoArg (\opts -> opts { outputStyle = LispStyle }))
            "print as a list of Lisp"
          , Option "g" []
            (ReqArg (\s opts -> opts { ghcOpts = s : ghcOpts opts }) "flag") "specify a ghc flag"
          ]

usage :: String
usage =    "ghc-modi version " ++ showVersion version ++ "\n"
        ++ "Usage:\n"
        ++ "\t ghc-modi [-l] [-b sep] [-g flag]\n"
        ++ "\t ghc-modi help\n"

parseArgs :: [OptDescr (Options -> Options)] -> [String] -> (Options, [String])
parseArgs spec argv
    = case getOpt Permute spec argv of
        (o,n,[]  ) -> (foldr id defaultOptions o, n)
        (_,_,errs) -> GE.throw (CmdArg errs)

----------------------------------------------------------------

data GHCModiError = CmdArg [String]
                  deriving (Show, Typeable)

instance GE.Exception GHCModiError

----------------------------------------------------------------

-- Running two GHC monad threads disables the handling of
-- C-c since installSignalHandlers is called twice, sigh.

main :: IO ()
main = handle [GE.Handler cmdHandler, GE.Handler someHandler] $
    go =<< parseArgs argspec <$> getArgs
  where
    handle = flip GE.catches
    LineSeparator lsc = lineSeparator defaultOptions
    cmdHandler (CmdArg e) = do
        putStr "ghc-modi:0:0:"
        let x = intercalate lsc e
        putStrLn x
        putStr $ usageInfo usage argspec
        putStrLn "NG"
    someHandler (SomeException e) = do
        putStr "ghc-modi:0:0:"
        let x = intercalate lsc $ lines $ show e
        putStrLn x
        putStrLn "NG"
    go (_,"help":_) = do
        putStr $ usageInfo usage argspec
        putStrLn "NG"
    go (opt,_) = do
        cradle0 <- findCradle
        let rootdir = cradleRootDir cradle0
            cradle = cradle0 { cradleCurrentDir = rootdir }
        setCurrentDirectory rootdir
        mvar <- liftIO newEmptyMVar
        mlibdir <- getSystemLibDir
        void $ forkIO $ setupDB cradle mlibdir opt mvar
        run cradle mlibdir opt $ loop opt S.empty mvar

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

loop :: Options -> Set FilePath -> MVar DB -> Logger -> Ghc ()
loop opt set mvar readLog  = do
    cmdArg <- liftIO getLine
    let (cmd,arg') = break (== ' ') cmdArg
        arg = dropWhile (== ' ') arg'
    (ret,ok,set') <- case cmd of
        "check" -> checkStx opt set arg readLog
        "find"  -> findSym  opt set arg mvar
        "lint"  -> lintStx  opt set arg
        "info"  -> showInfo opt set arg readLog
        "type"  -> showType opt set arg readLog
        _       -> return ([], False, set)
    let put = case outputStyle opt of
            LispStyle  -> putStr
            PlainStyle -> putStrLn
    liftIO $ put ret
    liftIO $ putStrLn $ if ok then "OK" else "NG"
    liftIO $ hFlush stdout
    when ok $ loop opt set' mvar readLog

----------------------------------------------------------------

checkStx :: Options
         -> Set FilePath
         -> FilePath
         -> Logger
         -> Ghc (String, Bool, Set FilePath)
checkStx opt set file readLog = do
    let add = not $ S.member file set
    GE.ghandle handler $ do
        mdel <- removeMainTarget
        when add $ addTargetFiles [file]
        void $ G.load LoadAllTargets
        msgs <- liftIO readLog
        let set1 = if add then S.insert file set else set
            set2 = case mdel of
                Nothing    -> set1
                Just delfl -> S.delete delfl set1
        let ret = convert opt msgs
        return (ret, True, set2)
  where
    handler :: SourceError -> Ghc (String, Bool, Set FilePath)
    handler err = do
        ret <- handleErrMsg opt err
        return (ret, False, set)
    removeMainTarget = do
        mx <- find isMain <$> G.getModuleGraph
        case mx of
            Nothing -> return Nothing
            Just x  -> do
                let mmainfile = G.ml_hs_file (G.ms_location x)
                    -- G.ms_hspp_file x is a temporary file with CPP.
                    -- this is a just fake.
                    mainfile = fromMaybe (G.ms_hspp_file x) mmainfile
                if mainfile == file then
                    return Nothing
                  else do
                    let target = TargetFile mainfile Nothing
                    G.removeTarget target
                    return $ Just mainfile
    isMain m = G.moduleNameString (G.moduleName (G.ms_mod m)) == "Main"

findSym :: Options -> Set FilePath -> String -> MVar DB
        -> Ghc (String, Bool, Set FilePath)
findSym opt set sym mvar = do
    db <- liftIO $ readMVar mvar
    let ret = convert opt $ fromMaybe [] (M.lookup sym db)
    return (ret, True, set)

lintStx :: Options -> Set FilePath -> FilePath
        -> Ghc (String, Bool, Set FilePath)
lintStx opt set optFile = liftIO $ E.handle handler $ do
    ret <-lintSyntax opt' file
    return (ret, True, set)
  where
    (opts,file) = parseLintOptions optFile
    hopts = if opts == "" then [] else read opts
    opt' = opt { hlintOpts = hopts }
    -- let's continue the session
    handler (SomeException e) = do
        print e
        return ("", True, set)

-- |
-- >>> parseLintOptions "[\"--ignore=Use camelCase\", \"--ignore=Eta reduce\"] file name"
-- (["--ignore=Use camelCase", "--ignore=Eta reduce"], "file name")
-- >>> parseLintOptions "file name"
-- ([], "file name")
parseLintOptions :: String -> (String, String)
parseLintOptions optFile = case brk (== ']') (dropWhile (/= '[') optFile) of
    ("","")      -> ([],   optFile)
    (opt',file') -> (opt', dropWhile (== ' ') file')
  where
    brk _ []         =  ([],[])
    brk p (x:xs')
        | p x        =  ([x],xs')
        | otherwise  =  let (ys,zs) = brk p xs' in (x:ys,zs)

showInfo :: Options
         -> Set FilePath
         -> FilePath
         -> Logger
         -> Ghc (String, Bool, Set FilePath)
showInfo opt set fileArg readLog = do
    let [file, expr] = words fileArg
    (_, _, set') <- checkStx opt set file readLog
    ret <- info opt file expr
    _ <- liftIO readLog
    return (ret, True, set')

showType :: Options
         -> Set FilePath
         -> FilePath
         -> Logger
         -> Ghc (String, Bool, Set FilePath)
showType opt set fileArg readLog = do
    let [file, line, column] = words fileArg
    (_, _, set') <- checkStx opt set file readLog
    ret <- types opt file (read line) (read column)
    _ <- liftIO readLog
    return (ret, True, set')
