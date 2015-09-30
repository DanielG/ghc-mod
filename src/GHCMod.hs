{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

module Main where

import Config (cProjectVersion)
import Control.Category
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Typeable (Typeable)
import Data.Version (showVersion)
import Data.Label
import Data.List
import Data.List.Split
import Data.Char (isSpace)
import Data.Maybe
import Exception
import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Internal hiding (MonadIO,liftIO)
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Monad
import Paths_ghc_mod
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..))
import qualified System.Console.GetOpt as O
import System.FilePath ((</>))
import System.Directory (setCurrentDirectory, getAppUserDataDirectory,
                        removeDirectoryRecursive)
import System.Environment (getArgs)
import System.IO
import System.Exit
import Text.PrettyPrint
import Prelude hiding ((.))

import Misc

progVersion :: String -> String
progVersion pf =
    "ghc-mod"++pf++" version " ++ showVersion version ++ " compiled by GHC "
                               ++ cProjectVersion ++ "\n"

ghcModVersion :: String
ghcModVersion = progVersion ""

ghcModiVersion :: String
ghcModiVersion = progVersion "i"

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
 "Usage: ghc-mod [OPTIONS...] COMMAND [CMD_ARGS...] \n\
 \*Global Options (OPTIONS)*\n\
 \    Global options can be specified before and after the command and\n\
 \    interspersed with command specific options\n\
 \\n"
   ++ (unlines $ indent <$> optionUsage indent globalArgSpec) ++
 "*Commands*\n\
 \    - version\n\
 \        Print the version of the program.\n\
 \\n\
 \    - help\n\
 \       Print this help message.\n\
 \\n\
 \    - list [FLAGS...] | modules [FLAGS...]\n\
 \        List all visible modules.\n\
 \      Flags:\n\
 \        -d\n\
 \            Print package modules belong to.\n\
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
 \        -h\n\
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
 \    - debugComponent [MODULE_OR_FILE...]\n\
 \        Debugging information related to cabal component resolution.\n\
 \\n\
 \    - boot\n\
 \         Internal command used by the emacs frontend.\n\
 \\n\
 \    - legacy-interactive\n\
 \         ghc-modi compatibility mode.\n"
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

ghcModStyle :: Style
ghcModStyle = style { lineLength = 80, ribbonsPerLine = 1.2 }

----------------------------------------------------------------

option :: [Char] -> [String] -> String -> ArgDescr a -> OptDescr a
option s l udsc dsc = Option s l dsc udsc

reqArg :: String -> (String -> a) -> ArgDescr a
reqArg udsc dsc = ReqArg dsc udsc

optArg :: String -> (Maybe String -> a) -> ArgDescr a
optArg udsc dsc = OptArg dsc udsc

intToLogLevel :: Int -> GmLogLevel
intToLogLevel = toEnum

globalArgSpec :: [OptDescr (Options -> Either [String] Options)]
globalArgSpec =
      [ option "v" ["verbose"] "Increase or set log level. (0-7)" $
          optArg "LEVEL" $ \ml o -> Right $ case ml of
              Nothing ->
                  modify (lOoptLogLevel . lOptOutput) increaseLogLevel o
              Just l ->
                  set (lOoptLogLevel . lOptOutput) (toEnum $ min 7 $ read l) o

      , option "s" [] "Be silent, set log level to 0" $
          NoArg $ \o -> Right $ set (lOoptLogLevel . lOptOutput) (toEnum 0) o

      , option "l" ["tolisp"] "Format output as an S-Expression" $
          NoArg $ \o -> Right $ set (lOoptStyle . lOptOutput) LispStyle o

      , option "b" ["boundary", "line-seperator"] "Output line separator"$
          reqArg "SEP" $ \s o -> Right $ set (lOoptLineSeparator . lOptOutput) (LineSeparator s) o

      , option "" ["line-prefix"] "Output line separator"$
          reqArg "OUT,ERR" $ \s o -> let
                [out, err] = splitOn "," s
              in Right $ set (lOoptLinePrefix . lOptOutput) (Just (out, err)) o

      , option "g" ["ghcOpt", "ghc-option"] "Option to be passed to GHC" $
          reqArg "OPT" $ \g o -> Right $
              o { optGhcUserOptions = g : optGhcUserOptions o }

{-
File map docs:

CLI options:
* `--map-file "file1.hs=file2.hs"` can be used to tell
    ghc-mod that it should take source code for `file1.hs` from `file2.hs`.
    `file1.hs` can be either full path, or path relative to project root.
    `file2.hs` has to be either relative to project root,
    or full path (preferred).
* `--map-file "file.hs"` can be used to tell ghc-mod that it should take
    source code for `file.hs` from stdin. File end marker is `\EOT\n`,
    i.e. `\x04\x0A`. `file.hs` may or may not exist, and should be
    either full path, or relative to project root.

Interactive commands:
* `map-file file.hs` -- tells ghc-modi to read `file.hs` source from stdin.
    Works the same as second form of `--map-file` CLI option.
* `unmap-file file.hs` -- unloads previously mapped file, so that it's
    no longer mapped. `file.hs` can be full path or relative to
    project root, either will work.

Exposed functions:
* `loadMappedFile :: FilePath -> FilePath -> GhcModT m ()` -- maps `FilePath`,
    given as first argument to take source from `FilePath` given as second
    argument. Works exactly the same as first form of `--map-file`
    CLI option.
* `loadMappedFileSource :: FilePath -> String -> GhcModT m ()` -- maps
    `FilePath`, given as first argument to have source as given
    by second argument. Works exactly the same as second form of `--map-file`
    CLI option, sans reading from stdin.
* `unloadMappedFile :: FilePath -> GhcModT m ()` -- unmaps `FilePath`, given as
    first argument, and removes any temporary files created when file was
    mapped. Works exactly the same as `unmap-file` interactive command
-}
      , option "" ["map-file"] "Redirect one file to another, --map-file \"file1.hs=file2.hs\"" $
          reqArg "OPT" $ \g o ->
             let m = case second (drop 1) $ span (/='=') g of
                       (s,"") -> (s, Nothing)
                       (f,t)  -> (f, Just t)
             in
             Right $ o { optFileMappings = m : optFileMappings o }

      , option "" ["with-ghc"] "GHC executable to use" $
          reqArg "PATH" $ \p o -> Right $ set (lGhcProgram . lOptPrograms) p o

      , option "" ["with-ghc-pkg"] "ghc-pkg executable to use (only needed when guessing from GHC path fails)" $
          reqArg "PATH" $ \p o -> Right $ set (lGhcPkgProgram . lOptPrograms) p o

      , option "" ["with-cabal"] "cabal-install executable to use" $
          reqArg "PATH" $ \p o -> Right $ set (lCabalProgram . lOptPrograms) p o

      , option "" ["with-stack"] "stack executable to use" $
          reqArg "PATH" $ \p o -> Right $ set (lStackProgram . lOptPrograms) p o

      , option "" ["version"] "print version information" $
          NoArg $ \_ -> Left ["version"]

      , option "" ["help"] "print this help message" $
          NoArg $ \_ -> Left ["help"]
  ]



parseGlobalArgs :: [String] -> Either InvalidCommandLine (Options, [String])
parseGlobalArgs argv
    = case O.getOpt' RequireOrder globalArgSpec argv of
        (o,r,u,[]) -> case foldr (=<<) (Right defaultOptions) o of
                        Right o' -> Right (o', u ++ r)
                        Left c -> Right (defaultOptions, c)
        (_,_,u,e)  -> Left $ InvalidCommandLine $ Right $
            "Parsing command line options failed: "
               ++ concat (e ++ map errUnrec u)
 where
   errUnrec :: String -> String
   errUnrec optStr = "unrecognized option `" ++ optStr ++ "'\n"

parseCommandArgs :: [OptDescr (Options -> Either [String] Options)]
                 -> [String]
                 -> Options
                 -> (Options, [String])
parseCommandArgs spec argv opts
    = case O.getOpt RequireOrder (globalArgSpec ++ spec) argv of
        (o,r,[])   -> case foldr (=<<) (Right opts) o of
                        Right o' -> (o', r)
                        Left c -> (defaultOptions, c)
        (_,_,errs) ->
            fatalError $ "Parsing command options failed: " ++ concat errs

----------------------------------------------------------------

data CmdError = UnknownCommand String
              | NoSuchFileError String
              | LibraryError GhcModError

                deriving (Show, Typeable)

instance Exception CmdError

data InteractiveOptions = InteractiveOptions {
      ghcModExtensions :: Bool
    }

handler :: IOish m => GhcModT m a -> GhcModT m a
handler = flip gcatches $
          [ GHandler $ \(FatalError msg) -> exitError msg
          , GHandler $ \e@(ExitSuccess) -> throw e
          , GHandler $ \e@(ExitFailure _) -> throw e
          , GHandler $ \(InvalidCommandLine e) -> do
                case e of
                  Left cmd ->
                      exitError $ "Usage for `"++cmd++"' command:\n\n"
                                  ++ (cmdUsage cmd usage) ++ "\n"
                                  ++ "ghc-mod: Invalid command line form."
                  Right msg -> exitError $ "ghc-mod: " ++ msg
          , GHandler $ \(SomeException e) -> exitError $ "ghc-mod: " ++ show e
          ]

main :: IO ()
main = do
    hSetEncoding stdout utf8
    args <- getArgs
    case parseGlobalArgs args of
      Left e -> throw e
      Right res@(globalOptions,_) -> catches (progMain res) [
            Handler $ \(e :: GhcModError) ->
              runGmOutT globalOptions $ exitError $ renderStyle ghcModStyle (gmeDoc e)
          ]

progMain :: (Options,[String]) -> IO ()
progMain (globalOptions,cmdArgs) = runGmOutT globalOptions $
    case globalCommands cmdArgs of
      Just s -> gmPutStr s
      Nothing -> wrapGhcCommands globalOptions cmdArgs

globalCommands :: [String] -> Maybe String
globalCommands (cmd:_)
    | cmd == "help"    = Just usage
    | cmd == "version" = Just ghcModVersion
globalCommands _       = Nothing

-- ghc-modi
legacyInteractive :: IOish m => GhcModT m ()
legacyInteractive = do
    opt <- options
    prepareCabalHelper
    tmpdir <- cradleTempDir <$> cradle
    gmo <- gmoAsk
    symdbreq <- liftIO $ newSymDbReq opt gmo tmpdir
    world <- getCurrentWorld
    legacyInteractiveLoop symdbreq world

bug :: IOish m => String -> GhcModT m ()
bug msg = do
  gmPutStrLn $ notGood $ "BUG: " ++ msg
  liftIO exitFailure

notGood :: String -> String
notGood msg = "NG " ++ escapeNewlines msg

escapeNewlines :: String -> String
escapeNewlines = replace "\n" "\\n" . replace "\\n" "\\\\n"

replace :: String -> String -> String -> String
replace needle replacement = intercalate replacement . splitOn needle

legacyInteractiveLoop :: IOish m
                      => SymDbReq -> World -> GhcModT m ()
legacyInteractiveLoop symdbreq world = do
    liftIO . setCurrentDirectory =<< cradleRootDir <$> cradle

    -- blocking
    cmdArg <- liftIO $ getLine

    -- after blocking, we need to see if the world has changed.

    changed <- didWorldChange world

    world' <- if changed
                then getCurrentWorld -- TODO: gah, we're hitting the fs twice
                else return world

    when changed $ do
        dropSession

    let (cmd':args') = split (keepDelimsR $ condense $ whenElt isSpace) cmdArg
        arg = concat args'
        cmd = dropWhileEnd isSpace cmd'
        args = dropWhileEnd isSpace `map` args'

    res <- flip gcatches interactiveHandlers $ case dropWhileEnd isSpace cmd of
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

        "map-file"   ->  liftIO getFileSourceFromStdin
                     >>= loadMappedFileSource arg
                     >>  return ""

        "unmap-file" ->  unloadMappedFile arg
                     >>  return ""

        "quit"   -> liftIO $ exitSuccess
        ""       -> liftIO $ exitSuccess
        _        -> fatalError $ "unknown command: `" ++ cmd ++ "'"

    gmPutStr res >> gmPutStrLn "OK" >> liftIO (hFlush stdout)
    legacyInteractiveLoop symdbreq world'
 where
   interactiveHandlers =
          [ GHandler $ \e@(FatalError _) -> throw e
          , GHandler $ \e@(ExitSuccess) -> throw e
          , GHandler $ \e@(ExitFailure _) -> throw e
          , GHandler $ \(SomeException e) -> gmErrStrLn (show e) >> return ""
          ]

getFileSourceFromStdin :: IO String
getFileSourceFromStdin = do
  let loop' acc = do
        line <- getLine
        if line == "\EOT"
        then return $ intercalate "\n" $ reverse $ ((init line):acc)
        else loop' (line:acc)
  loop' []


-- Someone please already rewrite the cmdline parsing code *weep* :'(
wrapGhcCommands :: (IOish m, GmOut m) => Options -> [String] -> m ()
wrapGhcCommands _opts []            = fatalError "No command given (try --help)"
wrapGhcCommands _opts ("root":_) = gmPutStr =<< rootInfo
wrapGhcCommands opts args = do
    handleGmError $ runGhcModT opts $ handler $ do
      forM_ (reverse $ optFileMappings opts) $
        uncurry loadMMappedFiles

      ghcCommands args
 where
   handleGmError action = do
     (e, _l) <- liftIO . evaluate =<< action
     case e of
       Right _ ->
           return ()
       Left ed ->
           exitError $ renderStyle ghcModStyle (gmeDoc ed)

   loadMMappedFiles from (Just to) = loadMappedFile from to
   loadMMappedFiles from (Nothing) = do
       src <- liftIO getFileSourceFromStdin
       loadMappedFileSource from src


ghcCommands :: IOish m => [String] -> GhcModT m ()
ghcCommands []         = fatalError "No command given (try --help)"
ghcCommands (cmd:args) = gmPutStr =<< action args
 where
   action = case cmd of
     _ | cmd == "list" || cmd == "modules" -> modulesCmd
     "lang"    -> languagesCmd
     "flag"    -> flagsCmd
     "browse"  -> browseCmd
     "check"   -> checkSyntaxCmd
     "expand"  -> expandTemplateCmd
     "debug"   -> debugInfoCmd
     "debug-component" -> componentInfoCmd
     "info"    -> infoCmd
     "type"    -> typesCmd
     "split"   -> splitsCmd
     "sig"     -> sigCmd
     "refine"  -> refineCmd
     "auto"    -> autoCmd
     "find"    -> findSymbolCmd
     "lint"    -> lintCmd
--     "root"    -> rootInfoCmd
     "doc"     -> pkgDocCmd
     "dumpsym" -> dumpSymbolCmd
     "boot"    -> bootCmd
     "legacy-interactive" -> legacyInteractiveCmd
--     "nuke-caches" -> nukeCachesCmd
     _         -> fatalError $ "unknown command: `" ++ cmd ++ "'"

newtype FatalError = FatalError String deriving (Show, Typeable)
instance Exception FatalError

newtype InvalidCommandLine = InvalidCommandLine (Either String String)
    deriving (Show, Typeable)
instance Exception InvalidCommandLine

exitError :: (MonadIO m, GmOut m) => String -> m a
exitError msg = gmErrStrLn (dropWhileEnd (=='\n') msg) >> liftIO exitFailure

fatalError :: String -> a
fatalError s = throw $ FatalError $ "ghc-mod: " ++ s

withParseCmd :: IOish m
             => [OptDescr (Options -> Either [String] Options)]
             -> ([String] -> GhcModT m a)
             -> [String]
             -> GhcModT m a
withParseCmd spec action args  = do
  (opts', rest) <- parseCommandArgs spec args <$> options
  withOptions (const opts') $ action rest

withParseCmd' :: (IOish m, ExceptionMonad m)
              => String
              -> [OptDescr (Options -> Either [String] Options)]
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
  debugInfoCmd, componentInfoCmd, infoCmd, typesCmd, splitsCmd, sigCmd,
  refineCmd, autoCmd, findSymbolCmd, lintCmd, pkgDocCmd,
  dumpSymbolCmd, bootCmd, legacyInteractiveCmd, nukeCachesCmd
  :: IOish m => [String] -> GhcModT m String

modulesCmd    = withParseCmd' "modules" s $ \[] -> modules
 where s = modulesArgSpec
languagesCmd  = withParseCmd' "lang"    [] $ \[] -> languages
flagsCmd      = withParseCmd' "flag"    [] $ \[] -> flags
debugInfoCmd  = withParseCmd' "debug"   [] $ \[] -> debugInfo
componentInfoCmd = withParseCmd' "debugComponent" [] $ \ts -> componentInfo ts
-- internal
bootCmd       = withParseCmd' "boot" [] $ \[] -> boot
nukeCachesCmd = withParseCmd' "nuke-caches" [] $ \[] -> nukeCaches >> return ""

dumpSymbolCmd     = withParseCmd' "dump" [] $ \[tmpdir] -> dumpSymbol tmpdir
findSymbolCmd     = withParseCmd' "find" [] $ \[sym]  -> findSymbol sym
pkgDocCmd         = withParseCmd' "doc"  [] $ \[mdl]  -> pkgDoc mdl
lintCmd           = withParseCmd' "lint" s  $ \[file] -> lint file
 where s = hlintArgSpec
browseCmd         = withParseCmd s $ \mdls -> concat <$> browse `mapM` mdls
 where s = browseArgSpec
checkSyntaxCmd    = withParseCmd [] $ checkAction checkSyntax
expandTemplateCmd = withParseCmd [] $ checkAction expandTemplate

typesCmd      = withParseCmd [] $ locAction  "type"  types
splitsCmd     = withParseCmd [] $ locAction  "split" splits
sigCmd        = withParseCmd [] $ locAction  "sig"    sig
autoCmd       = withParseCmd [] $ locAction  "auto"   auto
refineCmd     = withParseCmd [] $ locAction' "refine" refine

infoCmd       = withParseCmd [] $ action
  where action [file,_,expr] = info file $ Expression expr
        action [file,expr]   = info file $ Expression expr
        action _ = throw $ InvalidCommandLine (Left "info")

legacyInteractiveCmd = withParseCmd [] go
 where
   go [] =
       legacyInteractive >> return ""
   go ("help":[]) =
       return usage
   go ("version":[]) =
       return ghcModiVersion
   go _ = throw $ InvalidCommandLine (Left "legacy-interactive")

checkAction :: ([t] -> a) -> [t] -> a
checkAction _ []         = throw $ InvalidCommandLine (Right "No files given.")
checkAction action files = action files

locAction :: String -> (String -> Int -> Int -> a) -> [String] -> a
locAction _ action [file,_,line,col] = action file (read line) (read col)
locAction _ action [file,  line,col] = action file (read line) (read col)
locAction cmd _ _ = throw $ InvalidCommandLine (Left cmd)

locAction' :: String -> (String -> Int -> Int -> Expression -> a) -> [String] -> a
locAction' _ action [f,_,line,col,expr] = action f (read line) (read col) (Expression expr)
locAction' _ action [f,  line,col,expr] = action f (read line) (read col) (Expression expr)
locAction' cmd _ _ = throw $ InvalidCommandLine (Left cmd)


modulesArgSpec :: [OptDescr (Options -> Either [String] Options)]
modulesArgSpec =
    [ option "d" ["detailed"] "Print package modules belong to." $
             NoArg $ \o -> Right $ o { optDetailed = True }
    ]


hlintArgSpec :: [OptDescr (Options -> Either [String] Options)]
hlintArgSpec =
    [ option "h" ["hlintOpt"] "Option to be passed to hlint" $
             reqArg "hlintOpt" $ \h o -> Right $ o { optHlintOpts = h : optHlintOpts o }
    ]

browseArgSpec :: [OptDescr (Options -> Either [String] Options)]
browseArgSpec =
    [ option "o" ["operators"] "Also print operators." $
             NoArg $ \o -> Right $ o { optOperators = True }
    , option "d" ["detailed"] "Print symbols with accompanying signature." $
             NoArg $ \o -> Right $ o { optDetailed = True }
    , option "q" ["qualified"] "Qualify symbols" $
             NoArg $ \o -> Right $ o { optQualified = True }
    ]

nukeCaches :: IOish m => GhcModT m ()
nukeCaches = do
  chdir <- liftIO $ (</> "cabal-helper") <$> getAppUserDataDirectory "ghc-mod"
  c <- cradle

  when (isCabalHelperProject $ cradleProject c) $ do
    let root = cradleRootDir c
    let dist = cradleDistDir c
    liftIO $ (trySome . removeDirectoryRecursive) `mapM_` [chdir, root </> dist]

trySome :: IO a -> IO (Either SomeException a)
trySome = try
