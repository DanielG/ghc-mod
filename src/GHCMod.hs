{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Typeable
import Data.Version
import Language.Haskell.GhcMod
import Paths_ghc_mod
import Prelude
import System.Console.GetOpt
import System.Directory
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStr, hPutStrLn, stdout, stderr, hSetEncoding, utf8)

----------------------------------------------------------------

ghcOptHelp :: String
ghcOptHelp = " [-g GHC_opt1 -g GHC_opt2 ...] "

usage :: String
usage =    "ghc-mod version " ++ showVersion version ++ "\n"
        ++ "Usage:\n"
        ++ "\t ghc-mod list" ++ ghcOptHelp ++ "[-l]\n"
        ++ "\t ghc-mod lang [-l]\n"
        ++ "\t ghc-mod flag [-l]\n"
        ++ "\t ghc-mod browse" ++ ghcOptHelp ++ "[-l] [-o] [-d] <module> [<module> ...]\n"
        ++ "\t ghc-mod check" ++ ghcOptHelp ++ "<HaskellFiles...>\n"
        ++ "\t ghc-mod expand" ++ ghcOptHelp ++ "<HaskellFiles...>\n"
        ++ "\t ghc-mod debug" ++ ghcOptHelp ++ "<HaskellFile>\n"
        ++ "\t ghc-mod info" ++ ghcOptHelp ++ "<HaskellFile> <module> <expression>\n"
        ++ "\t ghc-mod type" ++ ghcOptHelp ++ "<HaskellFile> <module> <line-no> <column-no>\n"
        ++ "\t ghc-mod lint [-h opt] <HaskellFile>\n"
        ++ "\t ghc-mod boot\n"
        ++ "\t ghc-mod help\n"

----------------------------------------------------------------

argspec :: [OptDescr (Options -> Options)]
argspec = [ Option "l" ["tolisp"]
            (NoArg (\opts -> opts { outputStyle = LispStyle }))
            "print as a list of Lisp"
          , Option "h" ["hlintOpt"]
            (ReqArg (\h opts -> opts { hlintOpts = h : hlintOpts opts }) "hlintOpt")
            "hlint options"
          , Option "g" ["ghcOpt"]
            (ReqArg (\g opts -> opts { ghcOpts = g : ghcOpts opts }) "ghcOpt")
            "GHC options"
          , Option "o" ["operators"]
            (NoArg (\opts -> opts { operators = True }))
            "print operators, too"
          , Option "d" ["detailed"]
            (NoArg (\opts -> opts { detailed = True }))
            "print detailed info"
          , Option "s" ["sandbox"]
            (ReqArg (\s opts -> opts { sandbox = Just s }) "path")
            "specify cabal-dev sandbox (default 'cabal-dev`)"
          , Option "b" ["boundary"]
            (ReqArg (\s opts -> opts { lineSeparator = LineSeparator s }) "sep")
            "specify line separator (default is Nul string)"
          ]

parseArgs :: [OptDescr (Options -> Options)] -> [String] -> (Options, [String])
parseArgs spec argv
    = case getOpt Permute spec argv of
        (o,n,[]  ) -> (foldr id defaultOptions o, n)
        (_,_,errs) -> throw (CmdArg errs)

----------------------------------------------------------------

data GHCModError = SafeList
                 | TooManyArguments String
                 | NoSuchCommand String
                 | CmdArg [String]
                 | FileNotExist String deriving (Show, Typeable)

instance Exception GHCModError

----------------------------------------------------------------

main :: IO ()
main = flip catches handlers $ do
-- #if __GLASGOW_HASKELL__ >= 611
    hSetEncoding stdout utf8
-- #endif
    args <- getArgs
    let (opt',cmdArg) = parseArgs argspec args
    (strVer,ver) <- getGHCVersion
    cradle <- findCradle (sandbox opt') strVer
    let opt = adjustOpts opt' cradle ver
        cmdArg0 = cmdArg !. 0
        cmdArg1 = cmdArg !. 1
        cmdArg2 = cmdArg !. 2
        cmdArg3 = cmdArg !. 3
        cmdArg4 = cmdArg !. 4
        remainingArgs = tail cmdArg
        nArgs n f = if length remainingArgs == n
                        then f
                        else throw (TooManyArguments cmdArg0)
    res <- case cmdArg0 of
      "browse" -> concat <$> mapM (browseModule opt) remainingArgs
      "list"   -> listModules opt
      "check"  -> checkSyntax opt cradle remainingArgs
      "expand" -> checkSyntax opt { expandSplice = True } cradle remainingArgs
      "debug"  -> nArgs 1 $ debugInfo opt cradle strVer cmdArg1
      "type"   -> nArgs 4 $ typeExpr opt cradle cmdArg1 cmdArg2 (read cmdArg3) (read cmdArg4)
      "info"   -> nArgs 3 infoExpr opt cradle cmdArg1 cmdArg2 cmdArg3
      "lint"   -> nArgs 1 withFile (lintSyntax opt) cmdArg1
      "lang"   -> listLanguages opt
      "flag"   -> listFlags opt
      "boot"   -> do
         mods  <- listModules opt
         langs <- listLanguages opt
         flags <- listFlags opt
         pre   <- concat <$> mapM (browseModule opt) preBrowsedModules
         return $ mods ++ langs ++ flags ++ pre
      "help"   -> return $ usageInfo usage argspec
      cmd      -> throw (NoSuchCommand cmd)
    putStr res
  where
    handlers = [Handler (handleThenExit handler1), Handler (handleThenExit handler2)]
    handleThenExit handler = \e -> handler e >> exitFailure
    handler1 :: ErrorCall -> IO ()
    handler1 = print -- for debug
    handler2 :: GHCModError -> IO ()
    handler2 SafeList = printUsage
    handler2 (TooManyArguments cmd) = do
        hPutStrLn stderr $ "\"" ++ cmd ++ "\": Too many arguments"
        printUsage
    handler2 (NoSuchCommand cmd) = do
        hPutStrLn stderr $ "\"" ++ cmd ++ "\" not supported"
        printUsage
    handler2 (CmdArg errs) = do
        mapM_ (hPutStr stderr) errs
        printUsage
    handler2 (FileNotExist file) = do
        hPutStrLn stderr $ "\"" ++ file ++ "\" not found"
        printUsage
    printUsage = hPutStrLn stderr $ '\n' : usageInfo usage argspec
    withFile cmd file = do
        exist <- doesFileExist file
        if exist
            then cmd file
            else throw (FileNotExist file)
    xs !. idx
      | length xs <= idx = throw SafeList
      | otherwise = xs !! idx
    adjustOpts opt cradle ver = case mPkgConf of
            Nothing      -> opt
            Just pkgConf -> opt {
                ghcOpts = ghcPackageConfOptions ver pkgConf ++ ghcOpts opt
              }
      where
        mPkgConf = cradlePackageConf cradle

----------------------------------------------------------------

preBrowsedModules :: [String]
preBrowsedModules = [
    "Prelude"
  , "Control.Applicative"
  , "Control.Monad"
  , "Control.Exception"
  , "Data.Char"
  , "Data.List"
  , "Data.Maybe"
  , "System.IO"
  ]


ghcPackageConfOptions :: Int -> String -> [String]
ghcPackageConfOptions ver file
  | ver >= 706 = ["-package-db",   file, "-no-user-package-db"]
  | otherwise  = ["-package-conf", file, "-no-user-package-conf"]
