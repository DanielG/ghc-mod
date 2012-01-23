{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Browse
import CabalDev (modifyOptions)
import Check
import Control.Applicative
import Control.Exception
import Data.Typeable
import Data.Version
import Info
import Lang
import Flag
import Lint
import List
import Paths_ghc_mod
import Prelude
import System.Console.GetOpt
import System.Directory
import System.Environment (getArgs)
import System.IO (hPutStr, hPutStrLn, stderr)
import Types

----------------------------------------------------------------

ghcOptHelp :: String
ghcOptHelp = " [-g GHC_opt1 -g GHC_opt2 ...] "

usage :: String
usage =    "ghc-mod version " ++ showVersion version ++ "\n"
        ++ "Usage:\n"
        ++ "\t ghc-mod list" ++ ghcOptHelp ++ "[-l]\n"
        ++ "\t ghc-mod lang [-l]\n"
        ++ "\t ghc-mod flag [-l]\n"
        ++ "\t ghc-mod browse" ++ ghcOptHelp ++ "[-l] [-o] <module> [<module> ...]\n"
        ++ "\t ghc-mod check" ++ ghcOptHelp ++ "<HaskellFile>\n"
        ++ "\t ghc-mod type" ++ ghcOptHelp ++ "<HaskellFile> <module> <expression>\n"
        ++ "\t ghc-mod info" ++ ghcOptHelp ++ "<HaskellFile> <module> <expression>\n"
        ++ "\t ghc-mod lint [-h opt] <HaskellFile>\n"
        ++ "\t ghc-mod boot\n"
        ++ "\t ghc-mod help\n"

----------------------------------------------------------------

defaultOptions :: Options
defaultOptions = Options {
    convert = toPlain
  , hlintOpts = []
  , ghcOpts = []
  , operators = False
  }

argspec :: [OptDescr (Options -> Options)]
argspec = [ Option "l" ["tolisp"]
            (NoArg (\opts -> opts { convert = toLisp }))
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
          ]

parseArgs :: [OptDescr (Options -> Options)] -> [String] -> (Options, [String])
parseArgs spec argv
    = case getOpt Permute spec argv of
        (o,n,[]  ) -> (foldl (flip id) defaultOptions o, n)
        (_,_,errs) -> throw (CmdArg errs)

----------------------------------------------------------------

data GHCModError = SafeList
                 | NoSuchCommand String
                 | CmdArg [String]
                 | FileNotExist String deriving (Show, Typeable)

instance Exception GHCModError

----------------------------------------------------------------

main :: IO ()
main = flip catches handlers $ do
    args <- getArgs
    let (opt',cmdArg) = parseArgs argspec args
    res <- modifyOptions opt' >>= \opt -> case safelist cmdArg 0 of
      "browse" -> concat <$> mapM (browseModule opt) (tail cmdArg)
      "list"   -> listModules opt
      "check"  -> withFile (checkSyntax opt) (safelist cmdArg 1)
      "type"   -> withFile (typeExpr opt (safelist cmdArg 2) (safelist cmdArg 3)) (safelist cmdArg 1)
      "info"   -> withFile (infoExpr opt (safelist cmdArg 2) (safelist cmdArg 3)) (safelist cmdArg 1)
      "lint"   -> withFile (lintSyntax opt)  (safelist cmdArg 1)
      "lang"   -> listLanguages opt
      "flag"   -> listFlags opt
      "boot"   -> do
         mods  <- listModules opt
         langs <- listLanguages opt
         flags <- listFlags opt
         pre   <- concat <$> mapM (browseModule opt) preBrowsedModules
         return $ mods ++ langs ++ flags ++ pre
      cmd      -> throw (NoSuchCommand cmd)
    putStr res
  where
    handlers = [Handler handler1, Handler handler2]
    handler1 :: ErrorCall -> IO ()
    handler1 = print -- for debug
    handler2 :: GHCModError -> IO ()
    handler2 SafeList = printUsage
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
    safelist xs idx
      | length xs <= idx = throw SafeList
      | otherwise = xs !! idx

----------------------------------------------------------------
toLisp :: [String] -> String
toLisp ms = "(" ++ unwords quoted ++ ")\n"
    where
      quote x = "\"" ++ x ++ "\""
      quoted = map quote ms

toPlain :: [String] -> String
toPlain = unlines

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
