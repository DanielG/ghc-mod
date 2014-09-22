{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

-- Commands:
--  check <file>
--  find <symbol>
--  info <file> <expr>
--  type <file> <line> <column>
--  lint [hlint options] <file>
--     the format of hlint options is [String] because they may contain
--     spaces and also <file> may contain spaces.
--  boot
--  browse [<package>:]<module>
--  quit
--
-- Session separators:
--   OK -- success
--   NG -- failure

module Main where

import Config (cProjectVersion)
import Control.Applicative ((<$>))
import Control.Concurrent.Async (Async, async, wait)
import Control.Exception (SomeException(..), Exception)
import qualified Control.Exception as E
import Control.Monad (when)
import CoreMonad (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Data.Version (showVersion)
import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Internal
import Paths_ghc_mod
import System.Console.GetOpt
import System.Directory (getModificationTime, setCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode, exitFailure)
import System.IO (hFlush,stdout)

import Utils

----------------------------------------------------------------

type Logger = IO String

data World = World {
    worldCabalFileModificationTime :: Maybe UTCTime
  } deriving (Show, Eq)

type UnGetLine = IORef (Maybe String)

data Restart = Restart deriving (Show, Typeable)

instance Exception Restart

----------------------------------------------------------------

progVersion :: String
progVersion = "ghc-modi version " ++ showVersion version ++ " compiled by GHC " ++ cProjectVersion ++ "\n"

argspec :: [OptDescr (Options -> Options)]
argspec = [ Option "b" ["boundary"]
            (ReqArg (\s opts -> opts { lineSeparator = LineSeparator s }) "sep")
            "specify line separator (default is Nul string)"
          , Option "l" ["tolisp"]
            (NoArg (\opts -> opts { outputStyle = LispStyle }))
            "print as a list of Lisp"
          , Option "g" []
            (ReqArg (\s opts -> opts { ghcUserOptions = s : ghcUserOptions opts }) "flag") "specify a ghc flag"
          ]

usage :: String
usage =    progVersion
        ++ "Usage:\n"
        ++ "\t ghc-modi [-l] [-b sep] [-g flag]\n"
        ++ "\t ghc-modi version\n"
        ++ "\t ghc-modi help\n"

parseArgs :: [OptDescr (Options -> Options)] -> [String] -> (Options, [String])
parseArgs spec argv
    = case getOpt Permute spec argv of
        (o,n,[]  ) -> (foldr id defaultOptions o, n)
        (_,_,errs) -> E.throw (CmdArg errs)

----------------------------------------------------------------

data GHCModiError = CmdArg [String]
                  deriving (Show, Typeable)

instance Exception GHCModiError

----------------------------------------------------------------

-- Running two GHC monad threads disables the handling of
-- C-c since installSignalHandlers is called twice, sigh.

main :: IO ()
main = E.handle cmdHandler $
    go =<< parseArgs argspec <$> getArgs
  where
    cmdHandler (CmdArg _) = putStr $ usageInfo usage argspec
    go (_,"help":_) = putStr $ usageInfo usage argspec
    go (_,"version":_) = putStr progVersion
    go (opt,_) = emptyNewUnGetLine >>= run opt

run :: Options -> UnGetLine -> IO ()
run opt ref = flip E.catches handlers $ do
    cradle0 <- findCradle
    let rootdir = cradleRootDir cradle0
--        c = cradle0 { cradleCurrentDir = rootdir } TODO: ?????
    setCurrentDirectory rootdir
    -- Asynchronous db loading starts here.
    symdbreq <- newSymDbReq opt
    (res, _) <- runGhcModT opt $ getCurrentWorld >>= loop symdbreq ref
    case res of
        Right () -> return ()
        Left (GMECabalConfigure msg) -> do
            putStrLn $ notGood $ "cabal configure failed: " ++ show msg
            exitFailure
        Left e -> bug $ show e
  where
    -- this is just in case.
    -- If an error is caught here, it is a bug of GhcMod library.
    handlers = [ E.Handler (\(_ :: ExitCode) -> return ())
               , E.Handler (\(_ :: Restart) -> run opt ref)
               , E.Handler (\(SomeException e) -> bug $ show e) ]

getCurrentWorld :: IOish m => GhcModT m World
getCurrentWorld = do
    crdl <- cradle
    mmt <- case cradleCabalFile crdl of
        Just file -> liftIO $ Just <$> getModificationTime file
        Nothing   -> return Nothing
    return $ World mmt

bug :: String -> IO ()
bug msg = do
  putStrLn $ notGood $ "BUG: " ++ msg
  exitFailure

notGood :: String -> String
notGood msg = "NG " ++ escapeNewlines msg

escapeNewlines :: String -> String
escapeNewlines = replace "\n" "\\n" . replace "\\n" "\\\\n"

replace :: String -> String -> String -> String
replace needle replacement = intercalate replacement . splitOn needle

----------------------------------------------------------------

emptyNewUnGetLine :: IO UnGetLine
emptyNewUnGetLine = newIORef Nothing

ungetCommand :: IORef (Maybe a) -> a -> IO ()
ungetCommand ref cmd = writeIORef ref (Just cmd)

getCommand :: UnGetLine -> IO String
getCommand ref = do
    mcmd <- readIORef ref
    case mcmd of
        Nothing -> getLine
        Just cmd -> do
            writeIORef ref Nothing
            return cmd

loop :: IOish m => SymDbReq -> UnGetLine -> World -> GhcModT m ()
loop symdbreq ref world = do
    -- blocking
    cmdArg <- liftIO $ getCommand ref
    -- after blocking, we need to see if the world has changed.
    world' <- getCurrentWorld
    when (world /= world') $ do
        liftIO $ ungetCommand ref cmdArg
        E.throw Restart
    let (cmd,arg') = break (== ' ') cmdArg
        arg = dropWhile (== ' ') arg'
    (ret,ok) <- case cmd of
        "check"  -> checkStx arg
        "find"   -> findSym arg symdbreq
        "lint"   -> lintStx arg
        "info"   -> showInfo arg
        "type"   -> showType arg
        "split"  -> doSplit arg
        "sig"    -> doSig arg
        "refine" -> doRefine arg
        "auto"   -> doAuto arg
        "boot"   -> bootIt
        "browse" -> browseIt arg
        "quit"   -> return ("quit", False)
        ""       -> return ("quit", False)
        _        -> return ([], True)
    if ok then do
        liftIO $ putStr ret
        liftIO $ putStrLn "OK"
      else do
        liftIO $ putStrLn $ notGood ret
    liftIO $ hFlush stdout
    when ok $ loop symdbreq ref world

----------------------------------------------------------------

checkStx :: IOish m => FilePath -> GhcModT m (String, Bool)
checkStx file = do
    eret <- check [file]
    case eret of
        Right ret -> return (ret, True)
        Left ret  -> return (ret, True)

----------------------------------------------------------------

type SymDbReqAction = (Either GhcModError SymbolDb, GhcModLog)
data SymDbReq = SymDbReq (IORef (Async SymDbReqAction)) (IO SymDbReqAction)

newSymDbReq :: Options -> IO SymDbReq
newSymDbReq opt = do
    let act = runGhcModT opt loadSymbolDb
    req <- async act
    ref <- newIORef req
    return $ SymDbReq ref act

getDb :: IOish m => SymDbReq -> GhcModT m SymbolDb
getDb (SymDbReq ref _) = do
    req <- liftIO $ readIORef ref
    -- 'wait' really waits for the asynchronous action at the fist time.
    -- Then it reads a cached value from the second time.
    hoistGhcModT =<< liftIO (wait req)

checkDb :: IOish m => SymDbReq -> SymbolDb -> GhcModT m SymbolDb
checkDb (SymDbReq ref act) db = do
    outdated <- liftIO $ isOutdated db
    if outdated then do
        -- async and wait here is unnecessary because this is essentially
        -- synchronous. But Async can be used a cache.
        req <- liftIO $ async act
        liftIO $ writeIORef ref req
        hoistGhcModT =<< liftIO (wait req)
      else
        return db

findSym :: IOish m => Symbol -> SymDbReq -> GhcModT m (String, Bool)
findSym sym symdbreq = do
    db <- getDb symdbreq >>= checkDb symdbreq
    ret <- lookupSymbol sym db
    return (ret, True)

lintStx :: IOish m => FilePath -> GhcModT m (String, Bool)
lintStx optFile = do
    ret <- withOptions changeOpt $ lint file
    return (ret, True)
  where
    (opts,file) = parseLintOptions optFile
    hopts = if opts == "" then [] else read opts
    changeOpt o = o { hlintOpts = hopts }

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

----------------------------------------------------------------

showInfo :: IOish m => FilePath -> GhcModT m (String, Bool)
showInfo fileArg = do
    let [file, expr] = splitN 2 fileArg
    ret <- info file expr
    return (ret, True)

showType :: IOish m => FilePath -> GhcModT m (String, Bool)
showType fileArg  = do
    let [file, line, column] = splitN 3 fileArg
    ret <- types file (read line) (read column)
    return (ret, True)

doSplit :: IOish m => FilePath -> GhcModT m (String, Bool)
doSplit fileArg  = do
    let [file, line, column] = splitN 3 fileArg
    ret <- splits file (read line) (read column)
    return (ret, True)

doSig :: IOish m => FilePath -> GhcModT m (String, Bool)
doSig fileArg  = do
    let [file, line, column] = splitN 3 fileArg
    ret <- sig file (read line) (read column)
    return (ret, True)

doRefine :: IOish m => FilePath -> GhcModT m (String, Bool)
doRefine fileArg  = do
    let [file, line, column, expr] = splitN 4 fileArg
    ret <- refine file (read line) (read column) expr
    return (ret, True)

doAuto :: IOish m => FilePath -> GhcModT m (String, Bool)
doAuto fileArg  = do
    let [file, line, column] = splitN 3 fileArg
    ret <- auto file (read line) (read column)
    return (ret, True)

----------------------------------------------------------------

bootIt :: IOish m => GhcModT m (String, Bool)
bootIt = do
    ret <- boot
    return (ret, True)

browseIt :: IOish m => ModuleString -> GhcModT m (String, Bool)
browseIt mdl = do
    let (det,rest') = break (== ' ') mdl
        rest = dropWhile (== ' ') rest'
    ret <- if det == "-d"
               then withOptions setDetailed (browse rest)
               else browse mdl
    return (ret, True)
  where
    setDetailed opt = opt { detailed = True }
