{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

module Main where

import Config (cProjectVersion)
import MonadUtils (liftIO)
import Control.Applicative
import Control.Monad
import Data.Typeable (Typeable)
import Data.Version (showVersion)
import Data.Default
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char (isSpace)
import Exception
import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Internal
import Paths_ghc_mod
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..))
import qualified System.Console.GetOpt as O
import System.Directory (setCurrentDirectory)
import System.Environment (getArgs,getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stdout, stderr, hSetEncoding, utf8, hFlush)
import System.IO.Unsafe (unsafePerformIO)
import System.FilePath (takeFileName)
import System.Exit (ExitCode, exitSuccess)
import Text.PrettyPrint

import Misc



progVersion :: String
progVersion =
    progName ++ " version " ++ showVersion version ++ " compiled by GHC "
                            ++ cProjectVersion ++ "\n"

-- TODO: remove (ghc) version prefix!
progName :: String
progName = unsafePerformIO $ takeFileName <$> getProgName

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

usage :: String
usage =
    case progName of
      "ghc-modi" -> ghcModiUsage
      _ -> ghcModUsage

-- TODO: Generate the stuff below automatically
ghcModUsage :: String
ghcModUsage =
 "Usage: ghc-mod [OPTIONS...] COMMAND [OPTIONS...] \n\
 \*Global Options (OPTIONS)*\n\
 \    Global options can be specified before and after the command and\n\
 \    interspersed with command specific options\n\
 \\n"
   ++ (unlines $ indent <$> optionUsage indent globalArgSpec) ++
 "*Commands*\n\
 \    - version | --version\n\
 \        Print the version of the program.\n\
 \\n\
 \    - help | --help\n\
 \       Print this help message.\n\
 \\n\
 \    - list [FLAGS...] | modules [FLAGS...]\n\
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
 \    - root\n\
 \        Try to find the project directory. For Cabal projects this is the\n\
 \        directory containing the cabal file, for projects that use a cabal\n\
 \        sandbox but have no cabal file this is the directory containing the\n\
 \        cabal.sandbox.config file and otherwise this is the current\n\
 \        directory.\n\
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

ghcModiUsage :: String
ghcModiUsage =
 "Usage: ghc-modi [OPTIONS...] COMMAND\n\
 \*Options*\n"
   ++ (unlines $ indent <$> optionUsage indent globalArgSpec) ++
 "*Commands*\n\
 \    - version | --version\n\
 \        Print the version of the program.\n\
 \\n\
 \    - help | --help\n\
 \       Print this help message.\n"
 where
   indent = ("    "++)

cmdUsage :: String -> String -> String
cmdUsage cmd realUsage =
  let
      -- Find command head
      a = dropWhile (not . isCmdHead) $ lines realUsage
      -- Take til the end of the current command block
      b = flip takeWhile a $ \l ->
            all isSpace l || (isIndented l && (isCmdHead l || isNotCmdHead l))
      -- Drop extra newline from the end
      c = dropWhileEnd (all isSpace) b

      isIndented    = ("    " `isPrefixOf`)
      isNotCmdHead  = ( not .  ("    - " `isPrefixOf`))

      containsAnyCmdHead s = (("    - ") `isInfixOf` s)
      containsCurrCmdHead s = (("    - " ++ cmd) `isInfixOf` s)
      isCmdHead s =
          containsAnyCmdHead s &&
            or [ containsCurrCmdHead s
               , any (cmd `isPrefixOf`) (splitOn " | " s)
               ]

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


parseGlobalArgs :: [String] -> Either InvalidCommandLine (Options, [String])
parseGlobalArgs argv
    = case O.getOpt RequireOrder globalArgSpec argv of
        (o,r,[]  ) -> Right $ (foldr id defaultOptions o, r)
        (_,_,errs) -> Left $ InvalidCommandLine $ Right $
            "Parsing command line options failed: " ++ concat errs

parseCommandArgs :: [OptDescr (Options -> Options)]
                 -> [String]
                 -> Options
                 -> (Options, [String])
parseCommandArgs spec argv opts
    = case O.getOpt RequireOrder (globalArgSpec ++ spec) argv of
        (o,r,[])   -> (foldr id opts o, r)
        (_,_,errs) ->
            fatalError $ "Parsing command options failed: " ++ concat errs

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
                      exitError $ "Usage for `"++cmd++"' command:\n\n"
                                  ++ (cmdUsage cmd ghcModUsage) ++ "\n"
                                  ++ progName ++ ": Invalid command line form."
                  Right msg -> exitError $ progName ++ ": " ++ msg
          ]

main :: IO ()
main = handler $ do
    hSetEncoding stdout utf8
    args <- getArgs

    -- This doesn't handle --help and --version being given after any global
    -- options. To do that we'd have to fiddle with getOpt.
    case parseGlobalArgs args of
      Left e -> case globalCommands args of
                  Just s -> putStr s
                  Nothing -> throw e

      Right res@(_,cmdArgs) ->
          case globalCommands cmdArgs of
            Just s -> putStr s
            Nothing -> progMain res

progMain :: (Options,[String]) -> IO ()
progMain (globalOptions,cmdArgs) = do
    -- let (ghcArgs, modArgs) = second stripSeperator $ span (/="--") args
    --     _realGhcArgs = filter (/="--ghc-mod") ghcArgs

    --     (globalOptions,_cmdArgs) = parseGlobalArgs modArgs

    --     stripSeperator ("--":rest) = rest
    --     stripSeperator l = l

    case progName of
      "ghc-modi" -> do
          legacyInteractive globalOptions =<< emptyNewUnGetLine


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
                  (res,_) <- runGhcModT globalOptions $ ghcCommands cmdArgs
                  case res of
                    Right s -> putStr s
                    Left e -> exitError $ render (gmeDoc e)

              -- Obtain ghc options by letting ourselfs be executed by
              -- @cabal repl@
              -- TODO: need to do something about non-cabal projects
              -- exe <- ghcModExecutable
              -- let cabalArgs = ["repl", "-v0", "--with-ghc="++exe]
              --              ++ (("--ghc-option="++) `map` ("--ghc-mod":"--":args))

              -- print cabalArgs

              -- rawSystem "cabal" cabalArgs >>= exitWith



-- ghc-modi
legacyInteractive :: Options -> UnGetLine -> IO ()
legacyInteractive opt ref = flip catches handlers $ do
    (res,_) <- runGhcModT opt $ do
             symdbreq <- liftIO $ newSymDbReq opt
             world <- liftIO . getCurrentWorld =<< cradle
             legacyInteractiveLoop symdbreq ref world

    case res of
      Right () -> return ()
      Left e -> putStrLn $ notGood $ render (gmeDoc e)

 where
   handlers = [ Handler $ \Restart -> legacyInteractive opt ref ]

isExitCodeException :: SomeException -> Bool
isExitCodeException e = isJust mExitCode
 where
   mExitCode :: Maybe ExitCode
   mExitCode = fromException e


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


legacyInteractiveLoop :: IOish m
                      => SymDbReq -> UnGetLine -> World -> GhcModT m ()
legacyInteractiveLoop symdbreq ref world = do
    liftIO . setCurrentDirectory =<< cradleRootDir <$> cradle

    -- blocking
    cmdArg <- liftIO $ getCommand ref

    -- after blocking, we need to see if the world has changed.

    changed <- liftIO . didWorldChange world =<< cradle
    when changed $ do
        liftIO $ ungetCommand ref cmdArg
        throw Restart

    liftIO . prepareAutogen =<< cradle

    let (cmd':args') = split (keepDelimsR $ condense $ whenElt isSpace) cmdArg
        arg = concat args'
        cmd = dropWhileEnd isSpace cmd'
        args = dropWhileEnd isSpace `map` args'

    res <- case dropWhileEnd isSpace cmd of
        "check"  -> checkSyntaxCmd [arg]
        "lint"   -> lintCmd [arg]
        "find"    -> do
            db <- getDb symdbreq >>= checkDb symdbreq
            lookupSymbol arg db

        "info"   -> infoCmd [head args, concat $ tail args']
        "type"   -> typesCmd args
        "split"  -> splitsCmd args

        "sig"    -> sigCmd args
        "auto"   -> autoCmd args
        "refine" -> refineCmd args

        "boot"   -> bootCmd []
        "browse" -> browseCmd args

        "quit"   -> liftIO $ exitSuccess
        ""       -> liftIO $ exitSuccess
        _        -> fatalError $ "unknown command: `" ++ cmd ++ "'"

    liftIO $ putStr res >> putStrLn "OK" >> hFlush stdout
    legacyInteractiveLoop symdbreq ref world


globalCommands :: [String] -> Maybe String
globalCommands []      = Nothing
globalCommands (cmd:_) = case cmd of
    _ | cmd == "help"    || cmd == "--help"    -> Just usage
    _ | cmd == "version" || cmd == "--version" -> Just progVersion
    _                                          -> Nothing

ghcCommands :: IOish m => [String] -> GhcModT m String
ghcCommands []         = fatalError "No command given (try --help)"
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
fatalError s = throw $ FatalError $ progName ++ ": " ++ s

withParseCmd :: IOish m
             => [OptDescr (Options -> Options)]
             -> ([String] -> GhcModT m a)
             -> [String]
             -> GhcModT m a
withParseCmd spec action args  = do
  (opts', rest) <- parseCommandArgs spec args <$> options
  withOptions (const opts') $ action rest

withParseCmd' :: (IOish m, ExceptionMonad m)
              => String
              -> [OptDescr (Options -> Options)]
              -> ([String] -> GhcModT m a)
              -> [String]
              -> GhcModT m a
withParseCmd' cmd spec action args =
    catchArgs cmd $ withParseCmd spec action args

catchArgs :: (Monad m, ExceptionMonad m) => String -> m a -> m a
catchArgs cmd action =
    action `gcatch` \(PatternMatchFail _) ->
        throw $ InvalidCommandLine (Left cmd)

modulesCmd, languagesCmd, flagsCmd, browseCmd, checkSyntaxCmd, expandTemplateCmd,
  debugInfoCmd, infoCmd, typesCmd, splitsCmd, sigCmd, refineCmd, autoCmd,
  findSymbolCmd, lintCmd, rootInfoCmd, pkgDocCmd, dumpSymbolCmd, bootCmd
  :: IOish m => [String] -> GhcModT m String

modulesCmd    = withParseCmd' "modules" [] $ \[] -> modules
languagesCmd  = withParseCmd' "lang"    [] $ \[] -> languages
flagsCmd      = withParseCmd' "flag"    [] $ \[] -> flags
debugInfoCmd  = withParseCmd' "debug"   [] $ \[] -> debugInfo
rootInfoCmd   = withParseCmd' "root"    [] $ \[] -> rootInfo
-- internal
bootCmd       = withParseCmd' "boot" [] $ \[] -> boot

dumpSymbolCmd     = withParseCmd' "dump" [] $ \[tmpdir] -> dumpSymbol tmpdir
findSymbolCmd     = withParseCmd' "find" [] $ \[sym]  -> findSymbol sym
pkgDocCmd         = withParseCmd' "doc"  [] $ \[mdl]  -> pkgDoc mdl
lintCmd           = withParseCmd' "lint" s  $ \[file] -> lint file
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
