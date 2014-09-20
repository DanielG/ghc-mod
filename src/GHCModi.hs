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
import Data.List (find, intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Typeable (Typeable)
import Data.Version (showVersion)
import qualified GHC as G
import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Internal
import Paths_ghc_mod
import System.Console.GetOpt
import System.Directory (setCurrentDirectory)
import System.Environment (getArgs)
import System.IO (hFlush,stdout)
import System.Exit (ExitCode, exitFailure)
import Utils
import Data.Time.Clock (getCurrentTime)
import System.Directory (getCurrentDirectory, getDirectoryContents, getModificationTime)

----------------------------------------------------------------

type Logger = IO String

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
    go (opt,_) = flip E.catches handlers $ do
        cradle0 <- findCradle
        let rootdir = cradleRootDir cradle0
--            c = cradle0 { cradleCurrentDir = rootdir } TODO: ?????
        setCurrentDirectory rootdir
        symDb <- async $ runGhcModT opt loadSymbolDb
        (res, _) <- runGhcModT opt $ loop S.empty symDb =<< liftIO staleCabal

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
                   , E.Handler (\(SomeException e) -> bug $ show e) ]

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

-- huge hack, must be a better way

staleCabal :: IO (IO Bool)
staleCabal = do
  startTime <- getCurrentTime
  return $ do
    files <- filter ((=="labac.") . take 6 . reverse)
             <$> (getDirectoryContents =<< getCurrentDirectory)
    case files of
      (cab:_) -> (> startTime) <$> getModificationTime cab
      _ -> error "no cabal file in directory?"


----------------------------------------------------------------
loop :: IOish m => Set FilePath -> SymDbReq -> IO Bool -> GhcModT m ()
loop set symDbReq stale = do
    cmdArg <- liftIO getLine
    let (cmd',arg') = break (== ' ') cmdArg
        arg = dropWhile (== ' ') arg'
    cmd <- liftIO $ stale
           >>= \x -> return $ if x
                              then "quit"
                              else cmd'
    (ret,ok,set') <- case cmd of
        "check"  -> checkStx set arg
        "find"   -> findSym set arg symDbReq
        "lint"   -> lintStx set arg
        "info"   -> showInfo set arg
        "type"   -> showType set arg
        "split"  -> doSplit set arg
        "sig"    -> doSig set arg
        "refine" -> doRefine set arg
        "auto"   -> doAuto set arg
        "boot"   -> bootIt set
        "browse" -> browseIt set arg
        "quit"   -> return ("quit", False, set)
        ""       -> return ("quit", False, set)
        _        -> return ([], True, set)
    if ok then do
        liftIO $ putStr ret
        liftIO $ putStrLn "OK"
      else do
        liftIO $ putStrLn $ notGood ret
    liftIO $ hFlush stdout
    when ok $ loop set' symDbReq stale

----------------------------------------------------------------

checkStx :: IOish m
         => Set FilePath
         -> FilePath
         -> GhcModT m (String, Bool, Set FilePath)
checkStx set file = do
    set' <- newFileSet set file
    let files = S.toList set'
    eret <- check files
    case eret of
        Right ret -> return (ret, True, set')
        Left ret  -> return (ret, True, set) -- fxime: set

newFileSet :: IOish m => Set FilePath -> FilePath -> GhcModT m (Set FilePath)
newFileSet set file = do
    let set1
         | S.member file set = set
         | otherwise         = S.insert file set
    mx <- isSameMainFile file <$> getModSummaryForMain
    return $ case mx of
        Nothing       -> set1
        Just mainfile -> S.delete mainfile set1

getModSummaryForMain :: IOish m => GhcModT m (Maybe G.ModSummary)
getModSummaryForMain = find isMain <$> G.getModuleGraph
  where
    isMain m = G.moduleNameString (G.moduleName (G.ms_mod m)) == "Main"

isSameMainFile :: FilePath -> (Maybe G.ModSummary) -> Maybe FilePath
isSameMainFile _    Nothing  = Nothing
isSameMainFile file (Just x)
    | mainfile == file = Nothing
    | otherwise        = Just mainfile
  where
    mmainfile = G.ml_hs_file (G.ms_location x)
    -- G.ms_hspp_file x is a temporary file with CPP.
    -- this is a just fake.
    mainfile = fromMaybe (G.ms_hspp_file x) mmainfile

----------------------------------------------------------------

type SymDbReq = Async (Either GhcModError SymbolDb, GhcModLog)

findSym :: IOish m => Set FilePath -> String -> SymDbReq
        -> GhcModT m (String, Bool, Set FilePath)
findSym set sym dbReq = do
    db <- hoistGhcModT =<< liftIO (wait dbReq)
    ret <- lookupSymbol sym db
    return (ret, True, set)

lintStx :: IOish m => Set FilePath
        -> FilePath
        -> GhcModT m (String, Bool, Set FilePath)
lintStx set optFile = do
    ret <- withOptions changeOpt $ lint file
    return (ret, True, set)
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

showInfo :: IOish m
         => Set FilePath
         -> FilePath
         -> GhcModT m (String, Bool, Set FilePath)
showInfo set fileArg = do
    let [file, expr] = splitN 2 fileArg
    set' <- newFileSet set file
    ret <- info file expr
    return (ret, True, set')

showType :: IOish m
         => Set FilePath
         -> FilePath
         -> GhcModT m (String, Bool, Set FilePath)
showType set fileArg  = do
    let [file, line, column] = splitN 3 fileArg
    set' <- newFileSet set file
    ret <- types file (read line) (read column)
    return (ret, True, set')

doSplit :: IOish m
        => Set FilePath
        -> FilePath
        -> GhcModT m (String, Bool, Set FilePath)
doSplit set fileArg  = do
    let [file, line, column] = splitN 3 fileArg
    set' <- newFileSet set file
    ret <- splits file (read line) (read column)
    return (ret, True, set')

doSig :: IOish m
      => Set FilePath
      -> FilePath
      -> GhcModT m (String, Bool, Set FilePath)
doSig set fileArg  = do
    let [file, line, column] = splitN 3 fileArg
    set' <- newFileSet set file
    ret <- sig file (read line) (read column)
    return (ret, True, set')

doRefine :: IOish m
         => Set FilePath
         -> FilePath
         -> GhcModT m (String, Bool, Set FilePath)
doRefine set fileArg  = do
    let [file, line, column, expr] = splitN 4 fileArg
    set' <- newFileSet set file
    ret <- refine file (read line) (read column) expr
    return (ret, True, set')

doAuto :: IOish m
       => Set FilePath
       -> FilePath
       -> GhcModT m (String, Bool, Set FilePath)
doAuto set fileArg  = do
    let [file, line, column] = splitN 3 fileArg
    set' <- newFileSet set file
    ret <- auto file (read line) (read column)
    return (ret, True, set')

----------------------------------------------------------------

bootIt :: IOish m
       => Set FilePath
       -> GhcModT m (String, Bool, Set FilePath)
bootIt set = do
    ret <- boot
    return (ret, True, set)

browseIt :: IOish m
         => Set FilePath
         -> ModuleString
         -> GhcModT m (String, Bool, Set FilePath)
browseIt set mdl = do
    let (det,rest') = break (== ' ') mdl
        rest = dropWhile (== ' ') rest'
    ret <- if det == "-d"
               then withOptions setDetailed (browse rest)
               else browse mdl
    return (ret, True, set)
  where
    setDetailed opt = opt { detailed = True } 
