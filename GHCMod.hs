{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Browse
import CabalApi
import Check
import Control.Applicative
import Control.Exception
import Cradle
import Data.Typeable
import Data.Version
import Debug
import Flag
import Info
import Lang
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
        ++ "\t ghc-mod browse" ++ ghcOptHelp ++ "[-l] [-o] [-d] <module> [<module> ...]\n"
        ++ "\t ghc-mod check" ++ ghcOptHelp ++ "<HaskellFile>\n"
        ++ "\t ghc-mod expand" ++ ghcOptHelp ++ "<HaskellFile>\n"
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
          ]

parseArgs :: [OptDescr (Options -> Options)] -> [String] -> (Options, [String])
parseArgs spec argv
    = case getOpt Permute spec argv of
        (o,n,[]  ) -> (foldr id defaultOptions o, n)
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
    (strVer,ver) <- getGHCVersion
    cradle <- findCradle (sandbox opt') strVer
    let opt = adjustOpts opt' cradle ver
        cmdArg0 = cmdArg !. 0
        cmdArg1 = cmdArg !. 1
        cmdArg2 = cmdArg !. 2
        cmdArg3 = cmdArg !. 3
        cmdArg4 = cmdArg !. 4
    res <- case cmdArg0 of
      "browse" -> concat <$> mapM (browseModule opt) (tail cmdArg)
      "list"   -> listModules opt
      "check"  -> withFile (checkSyntax opt cradle) cmdArg1
      "expand" -> withFile (checkSyntax opt { expandSplice = True } cradle) cmdArg1
      "debug"  -> withFile (debugInfo opt cradle strVer) cmdArg1
      "type"   -> withFile (typeExpr opt cradle cmdArg2 (read cmdArg3) (read cmdArg4)) cmdArg1
      "info"   -> withFile (infoExpr opt cradle cmdArg2 cmdArg3) cmdArg1
      "lint"   -> withFile (lintSyntax opt) cmdArg1
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
  | ver >= 706 = ["-package-db",   file, "-no-user-package-conf"]
  | otherwise  = ["-package-conf", file, "-no-user-package-conf"]
