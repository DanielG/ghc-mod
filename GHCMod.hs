module Main where

import Browse
import Check
import Control.Applicative
import Control.Exception hiding (try)
import Data.List
import List
import Prelude hiding (catch)
import System.Console.GetOpt
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import System.IO
import System.Posix.Env

----------------------------------------------------------------

usage :: String
usage =    "ghc-mod version 0.2.0\n"
        ++ "Usage:\n"
        ++ "\t ghc-mod list\n"
        ++ "\t ghc-mod browse <module>\n"
        ++ "\t ghc-mod check <HaskellFile>\n"
        ++ "\t ghc-mod help\n"

----------------------------------------------------------------

data Options   = Options { optToLisp :: Bool
                         } deriving Show

defaultOptions :: Options
defaultOptions = Options { optToLisp = False
                         }

argspec :: [OptDescr (Options -> Options)]
argspec = [ Option ['l'] ["tolisp"]
            (NoArg (\opts -> opts { optToLisp = True }))
            "print as a list of Lisp"
          ]

parseArgs :: [OptDescr (Options -> Options)] -> [String] -> (Options, [String])
parseArgs spec argv
    = case getOpt Permute spec argv of
        (o,n,[]  ) -> (foldl (flip id) defaultOptions o, n)
        (_,_,errs) -> error $ concat errs ++ usageInfo usage argspec

----------------------------------------------------------------

main :: IO ()
main = flip catch handler $ do
    args <- getArgs
    setPath
    let (opt,cmdArg) = parseArgs argspec args
        transform = if optToLisp opt then toLisp else toPlain
        refine = transform . nub . sort
    case cmdArg !! 0 of
      cmd | cmd == "browse" -> refine <$> browseModule (cmdArg !! 1) >>= putStr
          | cmd == "list"   -> refine <$> listModules >>= putStr
          | cmd == "check"  -> checkSyntax (cmdArg !! 1) >>= putStr
      _                     -> error usage
    hFlush stdout
  where
    handler :: ErrorCall -> IO ()
    handler _ = putStr usage

setPath :: IO ()
setPath = do
  home <- getHomeDirectory
  mpath <- getEnv "PATH"
  let path = maybe "/usr/bin:/bin" id mpath
      newpath = "/usr/local/bin:/opt/local/bin:"
             ++ (home </> ".cabal/bin") ++ ":"
             ++ (home </> "bin") ++ ":"
             ++ path
  setEnv "PATH" newpath True

----------------------------------------------------------------
toLisp :: [String] -> String
toLisp ms = "(" ++ unwords quoted ++ ")\n"
    where
      quote x = "\"" ++ x ++ "\""
      quoted = map quote ms

toPlain :: [String] -> String
toPlain = unlines
