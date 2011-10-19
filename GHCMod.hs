{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Browse
import Check
import Control.Applicative
import Control.Exception
import Data.Typeable
import Info
import Lang
import Lint
import List
import Prelude
import System.Console.GetOpt
import System.Directory
import System.Environment (getArgs)
import System.IO (hPutStr, hPutStrLn, stderr)
import Types

----------------------------------------------------------------

usage :: String
usage =    "ghc-mod version 0.6.2\n"
        ++ "Usage:\n"
        ++ "\t ghc-mod list [-l]\n"
        ++ "\t ghc-mod lang [-l]\n"
        ++ "\t ghc-mod browse [-l] [-o] <module> [<module> ...]\n"
        ++ "\t ghc-mod check [-g GHC options] [-i inc] <HaskellFile>\n"
        ++ "\t ghc-mod type <HaskellFile> <module> <expression>\n"
        ++ "\t ghc-mod info <HaskellFile> <module> <expression>\n"
        ++ "\t ghc-mod lint [-h opt] <HaskellFile>\n"
        ++ "\t ghc-mod boot\n"
        ++ "\t ghc-mod help\n"

----------------------------------------------------------------

defaultOptions :: Options
defaultOptions = Options {
    convert = toPlain
  , hlintOpts = []
  , ghcOpts = []
  , checkIncludes = []
  , operators = False
  , packageConfs = []
  , useUserPackageConf = True
  }

argspec :: [OptDescr (Options -> Options)]
argspec = [ Option "l" ["tolisp"]
            (NoArg (\opts -> opts { convert = toLisp }))
            "print as a list of Lisp"
          , Option "h" ["hlintOpt"]
            (ReqArg (\h opts -> opts { hlintOpts = h : hlintOpts opts }) "hlintOpt")
            "hint to be ignored"
          , Option "g" ["ghcOpt"]
            (ReqArg (\g opts -> opts { ghcOpts = g : ghcOpts opts }) "ghcOpt")
            "extra GHC options"
          , Option "o" ["operators"]
            (NoArg (\opts -> opts { operators = True }))
            "print operators, too"
          , Option ""  ["package-conf"]
            (ReqArg (\p opts -> opts { packageConfs = p : packageConfs opts }) "path")
            "additional package database"
          , Option ""  ["no-user-package-conf"]
            (NoArg (\opts -> opts{ useUserPackageConf = False }))
            "do not read the user package database"
          , Option "i" ["include"]
            (ReqArg (\i opts -> opts{ checkIncludes = i : checkIncludes opts }) "include")
            "directory to include in search for modules"
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
    let (opt,cmdArg) = parseArgs argspec args
    res <- case safelist cmdArg 0 of
      "browse" -> concat <$> mapM (browseModule opt) (tail cmdArg)
      "list"   -> listModules opt
      "check"  -> withFile (checkSyntax opt) (safelist cmdArg 1)
      "type"   -> withFile (typeExpr opt (safelist cmdArg 2) (safelist cmdArg 3)) (safelist cmdArg 1)
      "info"   -> withFile (infoExpr opt (safelist cmdArg 2) (safelist cmdArg 3)) (safelist cmdArg 1)
      "lint"   -> withFile (lintSyntax opt)  (safelist cmdArg 1)
      "lang"   -> listLanguages opt
      "boot"   -> do
         mods  <- listModules opt
         langs <- listLanguages opt
         pre   <- browseModule opt "Prelude"
         return $ mods ++ langs ++ pre
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
