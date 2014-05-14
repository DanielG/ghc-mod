{-# LANGUAGE DeriveDataTypeable #-}

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
import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, readMVar)
import Control.Exception (SomeException(..), Exception)
import qualified Control.Exception as E
import Control.Monad (when, void)
import CoreMonad (liftIO)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Typeable (Typeable)
import Data.Version (showVersion)
import GHC (Ghc)
import qualified GHC as G
import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Ghc
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Internal
import Paths_ghc_mod
import System.Console.GetOpt
import System.Directory (setCurrentDirectory)
import System.Environment (getArgs)
import System.IO (hFlush,stdout)

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
            (ReqArg (\s opts -> opts { ghcOpts = s : ghcOpts opts }) "flag") "specify a ghc flag"
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
    go (opt,_) = E.handle someHandler $ do
        cradle0 <- findCradle
        let rootdir = cradleRootDir cradle0
            cradle = cradle0 { cradleCurrentDir = rootdir }
        setCurrentDirectory rootdir
        mvar <- liftIO newEmptyMVar
        mlibdir <- getSystemLibDir
        void $ forkIO $ setupDB cradle mlibdir opt mvar
        run cradle mlibdir opt $ loop opt S.empty mvar
      where
        -- this is just in case.
        -- If an error is caught here, it is a bug of GhcMod library.
        someHandler (SomeException e) = do
            putStrLn $ "NG " ++ replace (show e)

replace :: String -> String
replace [] = []
replace ('\n':xs) = ';' : replace xs
replace (x:xs)    =  x  : replace xs

----------------------------------------------------------------

run :: Cradle -> Maybe FilePath -> Options -> GhcMod a -> IO a
run _ _ opt body = runGhcMod opt $ do
    dflags <- G.getSessionDynFlags
    G.defaultCleanupHandler dflags body

----------------------------------------------------------------

setupDB :: Cradle -> Maybe FilePath -> Options -> MVar SymMdlDb -> IO ()
setupDB cradle mlibdir opt mvar = E.handle handler $ do
    db <- run cradle mlibdir opt getSymMdlDb
    putMVar mvar db
  where
    handler (SomeException _) = return () -- fixme: put emptyDb?

----------------------------------------------------------------

loop :: Options -> Set FilePath -> MVar SymMdlDb -> GhcMod ()
loop opt set mvar = do
    cmdArg <- liftIO getLine
    let (cmd,arg') = break (== ' ') cmdArg
        arg = dropWhile (== ' ') arg'
    (ret,ok,set') <- case cmd of
        "check"  -> checkStx opt set arg
        "find"   -> findSym set arg mvar
        "lint"   -> toGhcMod $ lintStx  opt set arg
        "info"   -> toGhcMod $ showInfo opt set arg
        "type"   -> toGhcMod $ showType opt set arg
        "boot"   -> bootIt set
        "browse" -> browseIt set arg
        "quit"   -> return ("quit", False, set)
        ""       -> return ("quit", False, set)
        _        -> return ([], True, set)
    if ok then do
        liftIO $ putStr ret
        liftIO $ putStrLn "OK"
      else do
        liftIO $ putStrLn $ "NG " ++ replace ret
    liftIO $ hFlush stdout
    when ok $ loop opt set' mvar

----------------------------------------------------------------

checkStx :: Options
         -> Set FilePath
         -> FilePath
         -> GhcMod (String, Bool, Set FilePath)
checkStx _ set file = do
    set' <- toGhcMod $ newFileSet set file
    let files = S.toList set'
    eret <- check files
    case eret of
        Right ret -> return (ret, True, set')
        Left ret  -> return (ret, True, set) -- fxime: set

newFileSet :: Set FilePath -> FilePath -> Ghc (Set FilePath)
newFileSet set file = do
    let set1
         | S.member file set = set
         | otherwise         = S.insert file set
    mx <- isSameMainFile file <$> getModSummaryForMain
    return $ case mx of
        Nothing       -> set1
        Just mainfile -> S.delete mainfile set1

getModSummaryForMain :: Ghc (Maybe G.ModSummary)
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

findSym :: Set FilePath -> String -> MVar SymMdlDb
        -> GhcMod (String, Bool, Set FilePath)
findSym set sym mvar = do
    db <- liftIO $ readMVar mvar
    opt <- options
    let ret = lookupSym' opt sym db
    return (ret, True, set)

lintStx :: Options -> Set FilePath -> FilePath
        -> Ghc (String, Bool, Set FilePath)
lintStx opt set optFile = liftIO $ do
    ret <-lintSyntax opt' file
    return (ret, True, set)
  where
    (opts,file) = parseLintOptions optFile
    hopts = if opts == "" then [] else read opts
    opt' = opt { hlintOpts = hopts }

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

showInfo :: Options
         -> Set FilePath
         -> FilePath
         -> Ghc (String, Bool, Set FilePath)
showInfo opt set fileArg = do
    let [file, expr] = words fileArg
    set' <- newFileSet set file
    ret <- info opt file expr
    return (ret, True, set')

showType :: Options
         -> Set FilePath
         -> FilePath
         -> Ghc (String, Bool, Set FilePath)
showType opt set fileArg  = do
    let [file, line, column] = words fileArg
    set' <- newFileSet set file
    ret <- types opt file (read line) (read column)
    return (ret, True, set')

----------------------------------------------------------------

bootIt :: Set FilePath
       -> GhcMod (String, Bool, Set FilePath)
bootIt set = do
    ret <- boot
    return (ret, True, set)

browseIt :: Set FilePath
         -> ModuleString
         -> GhcMod (String, Bool, Set FilePath)
browseIt set mdl = do
    ret <- browse mdl
    return (ret, True, set)
