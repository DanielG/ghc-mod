module Main where

import Browse
import Check
import Control.Applicative
import Control.Exception hiding (try)
import Lang
import List
import Param
import Prelude hiding (catch)
import System.Console.GetOpt
import System.Environment (getArgs)

----------------------------------------------------------------

usage :: String
usage =    "ghc-mod version 0.4.0\n"
        ++ "Usage:\n"
        ++ "\t ghc-mod list\n"
        ++ "\t ghc-mod lang\n"
        ++ "\t ghc-mod browse <module>\n"
        ++ "\t ghc-mod check <HaskellFile>\n"
        ++ "\t ghc-mod help\n"

----------------------------------------------------------------

defaultOptions :: Options
defaultOptions = Options { convert = toPlain }

argspec :: [OptDescr (Options -> Options)]
argspec = [ Option "l" ["tolisp"]
            (NoArg (\opts -> opts { convert = toLisp }))
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
    let (opt,cmdArg) = parseArgs argspec args
    res <- case head cmdArg of
      "browse" -> concat <$> mapM (browseModule opt) (tail cmdArg)
      "list"   -> listModules opt
      "check"  -> checkSyntax opt (cmdArg !! 1)
      "lang"   -> listLanguages opt
      _        -> error usage
    putStr res
  where
    handler :: ErrorCall -> IO ()
    handler _ = putStr usage

----------------------------------------------------------------
toLisp :: [String] -> String
toLisp ms = "(" ++ unwords quoted ++ ")\n"
    where
      quote x = "\"" ++ x ++ "\""
      quoted = map quote ms

toPlain :: [String] -> String
toPlain = unlines
