{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Config (cProjectVersion)
import Control.Applicative
import Control.Exception (Exception, Handler(..), catches, throw)
import Data.Typeable (Typeable)
import Data.Version (showVersion)
import Data.Default
import Data.List
import Data.Char (isSpace)
import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Internal
import Paths_ghc_mod
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..))
import qualified System.Console.GetOpt as O
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stdout, stderr, hSetEncoding, utf8)
--import System.Process (rawSystem)
--import System.Exit (exitWith)
import Text.PrettyPrint

----------------------------------------------------------------

progVersion :: String
progVersion =
    "ghc-mod version " ++ showVersion version ++ " compiled by GHC "
                       ++ cProjectVersion ++ "\n"

optionUsage :: (String -> String) -> [OptDescr a] -> [String]
optionUsage indent opts = concatMap optUsage opts
 where
   optUsage (Option so lo dsc udsc) =
       [ concat $ intersperse ", " $ addLabel `map` allFlags
       , indent $ udsc
       , ""
       ]
    where
      allFlags = shortFlags ++ longFlags
      shortFlags = (('-':) . return) `map` so :: [String]
      longFlags  = ("--"++) `map` lo

      addLabel f@('-':'-':_) = f ++ flagLabel "="
      addLabel f@('-':_)     = f ++ flagLabel " "
      addLabel _ = undefined

      flagLabel s =
          case dsc of
            NoArg  _ -> ""
            ReqArg _ label -> s ++ label
            OptArg _ label -> s ++ "["++label++"]"

-- TODO: Generate the stuff below automatically
usage :: String
usage =
 "Usage: ghc-mod [OPTIONS...] COMMAND [OPTIONS...] \n\
 \*Global Options (OPTIONS)*\n\
 \    Global options can be specified before and after the command and\n\
 \    interspersed with command specific options\n\
 \\n"
   ++ (unlines $ indent <$> optionUsage indent globalArgSpec) ++
 "*Commands*\n\
 \    - version\n\
 \        Print the version of the program.\n\
 \\n\
 \    - help | --help\n\
 \       Print this help message.\n\
 \\n\
 \    - list [FLAGS...]\n\
 \        List all visible modules.\n\
 \      Flags:\n\
 \        -d\n\
 \            Also print the modules' package.\n\
 \\n\
 \    - lang\n\
 \        List all known GHC language extensions.\n\
 \\n\
 \    - flag\n\
 \        List GHC -f<bla> flags.\n\
 \\n\
 \    - browse [FLAGS...] [PACKAGE:]MODULE...\n\
 \        List symbols in a module.\n\
 \      Flags:\n\
 \        -o\n\
 \            Also print operators.\n\
 \        -d\n\
 \            Print symbols with accompanying signatures.\n\
 \        -q\n\
 \            Qualify symbols.\n\
 \\n\
 \    - check FILE...\n\
 \        Load the given files using GHC and report errors/warnings, but\n\
 \        don't produce output files.\n\
 \\n\
 \    - expand FILE...\n\
 \        Like `check' but also pass `-ddump-splices' to GHC.\n\
 \\n\
 \    - info   FILE [MODULE] EXPR\n\
 \        Look up an identifier in the context of FILE (like ghci's `:info')\n\
 \        MODULE is completely ignored and only allowed for backwards\n\
 \        compatibility.\n\
 \\n\
 \    - type FILE [MODULE] LINE COL\n\
 \        Get the type of the expression under (LINE,COL).\n\
 \\n\
 \    - split FILE [MODULE] LINE COL\n\
 \        Split a function case by examining a type's constructors.\n\
 \\n\
 \        For example given the following code snippet:\n\
 \\n\
 \            f :: [a] -> a\n\
 \            f x = _body\n\
 \\n\
 \        would be replaced by:\n\
 \\n\
 \            f :: [a] -> a\n\
 \            f [] = _body\n\
 \            f (x:xs) = _body\n\
 \\n\
 \        (See https://github.com/kazu-yamamoto/ghc-mod/pull/274)\n\
 \\n\
 \    - sig FILE MODULE LINE COL\n\
 \        Generate initial code given a signature.\n\
 \\n\
 \        For example when (LINE,COL) is on the signature in the following\n\
 \        code snippet:\n\
 \\n\
 \            func :: [a] -> Maybe b -> (a -> b) -> (a,b)\n\
 \\n\
 \        ghc-mod would add the following on the next line:\n\
 \\n\
 \            func x y z f = _func_body\n\
 \\n\
 \        (See: https://github.com/kazu-yamamoto/ghc-mod/pull/274)\n\
 \\n\
 \    - refine FILE MODULE LINE COL EXPR\n\
 \        Refine the typed hole at (LINE,COL) given EXPR.\n\
 \\n\
 \        For example if EXPR is `filter', which has type `(a -> Bool) -> [a]\n\
 \          -> [a]' and (LINE,COL) is on the hole `_body' in the following\n\
 \        code snippet:\n\
 \\n\
 \            filterNothing :: [Maybe a] -> [a]\n\
 \            filterNothing xs = _body\n\
 \\n\
 \        ghc-mod changes the code to get a value of type `[a]', which\n\
 \        results in:\n\
 \\n\
 \            filterNothing xs = filter _body_1 _body_2\n\
 \\n\
 \        (See also: https://github.com/kazu-yamamoto/ghc-mod/issues/311)\n\
 \\n\
 \    - auto FILE MODULE LINE COL\n\
 \        Try to automatically fill the contents of a hole.\n\
 \\n\
 \    - find SYMBOL\n\
 \        List all modules that define SYMBOL.\n\
 \\n\
 \    - lint FILE\n\
 \        Check files using `hlint'.\n\
 \      Flags:\n\
 \        -l\n\
 \            Option to be passed to hlint.\n\
 \\n\
 \    - root FILE\n\
 \       Try to find the project directory given FILE. For Cabal\n\
 \       projects this is the directory containing the cabal file, for\n\
 \       projects that use a cabal sandbox but have no cabal file this is the\n\
 \       directory containing the sandbox and otherwise this is the directory\n\
 \       containing FILE.\n\
 \\n\
 \    - doc MODULE\n\
 \        Try finding the html documentation directory for the given MODULE.\n\
 \\n\
 \    - debug\n\
 \        Print debugging information. Please include the output in any bug\n\
 \        reports you submit.\n\
 \\n\
 \    - boot\n\
 \         Internal command used by the emacs frontend.\n"
 -- "\n\
 -- \The following forms are supported so ghc-mod can be invoked by\n\
 -- \`cabal repl':\n\
 -- \\n\
 -- \     ghc-mod --make GHC_OPTIONS\n\
 -- \         Pass all options through to the GHC executable.\n\
 -- \\n\
 -- \     ghc-mod --interactive GHC_OPTIONS [--ghc-mod]\n\
 -- \         Start ghci emulation mode. GHC_OPTIONS are passed to the\n\
 -- \         GHC API. If `--ghc-mod' is given ghc-mod specific extensions\n\
 -- \         are enabled.\n"
 where
   indent = ("    "++)

cmdUsage :: String -> String -> String
cmdUsage cmd s =
  let
      -- Find command head
      a = dropWhile (not . (("    - " ++ cmd) `isInfixOf`)) $ lines s
      -- Take til the end of the current command block
      b = flip takeWhile a $ \l ->
           all isSpace l || (isIndented l && (isCurrCmdHead l || isNotCmdHead l))
      -- Drop extra newline from the end
      c = dropWhileEnd (all isSpace) b

      isIndented    = ("    " `isPrefixOf`)
      isNotCmdHead  = ( not .  ("    - " `isPrefixOf`))
      isCurrCmdHead = (("    - " ++ cmd) `isPrefixOf`)

      unindent (' ':' ':' ':' ':l) = l
      unindent l = l
  in unlines $ unindent <$> c
----------------------------------------------------------------

option :: [Char] -> [String] -> String -> ArgDescr a -> OptDescr a
option s l udsc dsc = Option s l dsc udsc

reqArg :: String -> (String -> a) -> ArgDescr a
reqArg udsc dsc = ReqArg dsc udsc

globalArgSpec :: [OptDescr (Options -> Options)]
globalArgSpec =
      [ option "v" ["verbose"] "Be more verbose." $
               NoArg $ \o -> o { ghcUserOptions = "-v" : ghcUserOptions o }

      , option "l" ["tolisp"] "Format output as an S-Expression" $
               NoArg $ \o -> o { outputStyle = LispStyle }

      , option "b" ["boundary"] "Output line separator"$
               reqArg "SEP" $ \s o -> o { lineSeparator = LineSeparator s }

      , option "g" ["ghcOpt"] "Option to be passed to GHC" $
               reqArg "OPT" $ \g o ->
                   o { ghcUserOptions = g : ghcUserOptions o }

      , option "" ["with-ghc"] "GHC executable to use" $
               reqArg "PROG" $ \p o -> o { ghcProgram = p }

      , option "" ["with-cabal"] "cabal-install executable to use" $
               reqArg "PROG" $ \p o -> o { cabalProgram = p }
  ]

parseGlobalArgs ::[String] -> (Options, [String])
parseGlobalArgs argv
    = case O.getOpt RequireOrder globalArgSpec argv of
        (o,r,[]  ) -> (foldr id defaultOptions o, r)
        (_,_,errs) ->
            fatalError $ "Parsing command line options failed: \n" ++ concat errs

parseCommandArgs :: [OptDescr (Options -> Options)]
                 -> [String]
                 -> Options
                 -> (Options, [String])
parseCommandArgs spec argv opts
    = case O.getOpt RequireOrder (globalArgSpec ++ spec) argv of
        (o,r,[])   -> (foldr id opts o, r)
        (_,_,errs) ->
            fatalError $ "Parsing command options failed: \n" ++ concat errs

----------------------------------------------------------------

data CmdError = UnknownCommand String
              | NoSuchFileError String
              | LibraryError GhcModError

                deriving (Show, Typeable)

instance Exception CmdError

----------------------------------------------------------------

data InteractiveOptions = InteractiveOptions {
      ghcModExtensions :: Bool
    }

instance Default InteractiveOptions where
    def = InteractiveOptions False

handler :: IO a -> IO a
handler = flip catches $
          [ Handler $ \(FatalError msg) -> exitError msg
          , Handler $ \(InvalidCommandLine e) -> do
                case e of
                  Left cmd ->
                      exitError $ (cmdUsage cmd usage)
                               ++ "\nghc-mod: Invalid command line form."
                  Right msg -> exitError msg
          ]

main :: IO ()
main = handler $ do
    hSetEncoding stdout utf8
    args <- getArgs

    -- let (ghcArgs, modArgs) = second stripSeperator $ span (/="--") args
    --     _realGhcArgs = filter (/="--ghc-mod") ghcArgs

    --     (globalOptions,_cmdArgs) = parseGlobalArgs modArgs

    --     stripSeperator ("--":rest) = rest
    --     stripSeperator l = l

    case args of
      _
          -- | "--numeric-version" `elem` ghcArgs || "--make" `elem` ghcArgs -> do
          --     rawSystem (ghcProgram globalOptions) realGhcArgs >>= exitWith

          -- | "--interactive" `elem` ghcArgs -> do
          --     let interactiveOptions = if "--ghc-mod" `elem` ghcArgs
          --                              then def { ghcModExtensions = True }
          --                              else def

          --     -- TODO: pass ghcArgs' to ghc API
          --     putStrLn "\ninteractive\n"
          --     --print realGhcArgs
          --     (res, _) <- runGhcModT globalOptions $ undefined
          --     case res of
          --       Right s -> putStr s
          --       Left e -> exitError $ render (gmeDoc e)


          | otherwise -> do
              let (globalOptions,cmdArgs) = parseGlobalArgs args
              res      <- simpleCommands cmdArgs
              putStr =<< case res of
                Just s -> return s
                Nothing -> do
                  (res',_) <- runGhcModT globalOptions $ ghcCommands cmdArgs
                  case res' of
                    Right s -> return s
                    Left e -> exitError $ render (gmeDoc e)

              -- Obtain ghc options by letting ourselfs be executed by
              -- @cabal repl@
              -- TODO: need to do something about non-cabal projects
              -- exe <- ghcModExecutable
              -- let cabalArgs = ["repl", "-v0", "--with-ghc="++exe]
              --              ++ (("--ghc-option="++) `map` ("--ghc-mod":"--":args))

              -- print cabalArgs

              -- rawSystem "cabal" cabalArgs >>= exitWith

simpleCommands :: [String] -> IO (Maybe String)
simpleCommands []      = return Nothing
simpleCommands (cmd:_) = return $ case cmd of
    _ | cmd == "help" || cmd == "--help"  -> Just usage
    "version" -> Just progVersion
    _         -> Nothing

ghcCommands :: IOish m => [String] -> GhcModT m String
ghcCommands []         = fatalError "No command given (try --help)\n"
ghcCommands (cmd:args) = fn args
 where
   fn = case cmd of
     _ | cmd == "list" || cmd == "modules" -> modulesCmd
     "lang"    -> languagesCmd
     "flag"    -> flagsCmd
     "browse"  -> browseCmd
     "check"   -> checkSyntaxCmd
     "expand"  -> expandTemplateCmd
     "debug"   -> debugInfoCmd
     "info"    -> infoCmd
     "type"    -> typesCmd
     "split"   -> splitsCmd
     "sig"     -> sigCmd
     "refine"  -> refineCmd
     "auto"    -> autoCmd
     "find"    -> findSymbolCmd
     "lint"    -> lintCmd
     "root"    -> rootInfoCmd
     "doc"     -> pkgDocCmd
     "dumpsym" -> dumpSymbolCmd
     "boot"    -> bootCmd
     _         -> fatalError $ "unknown command: `" ++ cmd ++ "'"

newtype FatalError = FatalError String deriving (Show, Typeable)
instance Exception FatalError

newtype InvalidCommandLine = InvalidCommandLine (Either String String)
    deriving (Show, Typeable)
instance Exception InvalidCommandLine

exitError :: String -> IO a
exitError msg = hPutStrLn stderr msg >> exitFailure

fatalError :: String -> a
fatalError s = throw $ FatalError $ "ghc-mod: " ++ s

withParseCmd :: IOish m
             => [OptDescr (Options -> Options)]
             -> ([String] -> GhcModT m a)
             -> [String]
             -> GhcModT m a
withParseCmd spec action args  = do
  (opts', rest) <- parseCommandArgs spec args <$> options
  withOptions (const opts') $ action rest

modulesCmd, languagesCmd, flagsCmd, browseCmd, checkSyntaxCmd, expandTemplateCmd,
  debugInfoCmd, infoCmd, typesCmd, splitsCmd, sigCmd, refineCmd, autoCmd,
  findSymbolCmd, lintCmd, rootInfoCmd, pkgDocCmd, dumpSymbolCmd, bootCmd
  :: IOish m => [String] -> GhcModT m String

modulesCmd    = withParseCmd [] $ \[] -> modules
languagesCmd  = withParseCmd [] $ \[] -> languages
flagsCmd      = withParseCmd [] $ \[] -> flags
debugInfoCmd  = withParseCmd [] $ \[] -> debugInfo
rootInfoCmd   = withParseCmd [] $ \[] -> rootInfo
-- internal
dumpSymbolCmd = withParseCmd [] $ \[] -> dumpSymbol
bootCmd       = withParseCmd [] $ \[] -> boot

findSymbolCmd     = withParseCmd [] $ \[sym]  -> findSymbol sym
pkgDocCmd         = withParseCmd [] $ \[mdl]  -> pkgDoc mdl
lintCmd           = withParseCmd s  $ \[file] -> lint file
 where s = hlintArgSpec
browseCmd         = withParseCmd s  $ \mdls   -> concat <$> browse `mapM` mdls
 where s = browseArgSpec
checkSyntaxCmd    = withParseCmd [] $ checkAction checkSyntax
expandTemplateCmd = withParseCmd [] $ checkAction expandTemplate

typesCmd      = withParseCmd [] $ locAction  "type"  types
splitsCmd     = withParseCmd [] $ locAction  "split" splits
sigCmd        = withParseCmd [] $ locAction  "sig"    sig
autoCmd       = withParseCmd [] $ locAction  "auto"   auto
refineCmd     = withParseCmd [] $ locAction' "refine" refine

infoCmd       = withParseCmd [] $ action
  where action [file,_,expr] = info file expr
        action [file,expr]   = info file expr
        action _ = throw $ InvalidCommandLine (Left "info")

checkAction :: ([t] -> a) -> [t] -> a
checkAction _ []         = throw $ InvalidCommandLine (Right "No files given.")
checkAction action files = action files

locAction :: String -> (String -> Int -> Int -> a) -> [String] -> a
locAction _ action [file,_,line,col] = action file (read line) (read col)
locAction _ action [file,  line,col] = action file (read line) (read col)
locAction cmd _ _ = throw $ InvalidCommandLine (Left cmd)

locAction' :: String -> (String -> Int -> Int -> String -> a) -> [String] -> a
locAction' _ action [f,_,line,col,expr] = action f (read line) (read col) expr
locAction' _ action [f,  line,col,expr] = action f (read line) (read col) expr
locAction' cmd _ _ = throw $ InvalidCommandLine (Left cmd)

hlintArgSpec :: [OptDescr (Options -> Options)]
hlintArgSpec =
    [ option "h" ["hlintOpt"] "Option to be passed to hlint" $
             reqArg "hlintOpt" $ \h o -> o { hlintOpts = h : hlintOpts o }
    ]
browseArgSpec :: [OptDescr (Options -> Options)]
browseArgSpec =
    [ option "o" ["operators"] "Also print operators." $
             NoArg $ \o -> o { operators = True }
    , option "d" ["detailed"] "Print symbols with accompanying signature." $
             NoArg $ \o -> o { detailed = True }
    , option "q" ["qualified"] "Qualify symbols" $
             NoArg $ \o -> o { qualified = True }
    ]
