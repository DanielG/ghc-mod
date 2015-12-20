{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

module Main where

import Control.Category
import Control.Applicative
import Control.Monad
import Data.Typeable (Typeable)
import Data.List
import Data.List.Split
import Data.Maybe
import Exception
import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Internal hiding (MonadIO,liftIO)
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Monad
import System.FilePath ((</>))
import System.Directory (setCurrentDirectory, getAppUserDataDirectory,
                        removeDirectoryRecursive)
import System.IO
import System.Exit
import Text.PrettyPrint hiding ((<>))
import Prelude hiding ((.))
import GHCMod.Options

import Misc

ghcModStyle :: Style
ghcModStyle = style { lineLength = 80, ribbonsPerLine = 1.2 }

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
          , GHandler $ \(SomeException e) -> exitError $ "ghc-mod: " ++ show e
          ]

main :: IO ()
main = do
    hSetEncoding stdout utf8
    parseArgs >>= \res@(globalOptions, _) ->
      catches (progMain res) [
              Handler $ \(e :: GhcModError) ->
                runGmOutT globalOptions $ exitError $ renderStyle ghcModStyle (gmeDoc e)
            ]

progMain :: (Options, GhcModCommands) -> IO ()
progMain (globalOptions, commands) = runGmOutT globalOptions $
  wrapGhcCommands globalOptions commands

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
    cmdArg <- liftIO getLine

    -- after blocking, we need to see if the world has changed.

    changed <- didWorldChange world

    world' <- if changed
                then getCurrentWorld -- TODO: gah, we're hitting the fs twice
                else return world

    when changed dropSession

    res <- flip gcatches interactiveHandlers $ do
      pargs <- either (throw . InvalidCommandLine . Right) return
              $ parseArgsInteractive cmdArg
      case pargs of
        CmdFind symbol ->
            lookupSymbol symbol =<< checkDb symdbreq =<< getDb symdbreq

        CmdMapFile f   ->  liftIO getFileSourceFromStdin
                       >>= loadMappedFileSource f
                       >>  return ""

        CmdUnmapFile f -> unloadMappedFile f
                       >> return ""

        CmdQuit        -> liftIO exitSuccess
        -- other commands are handled here
        x              -> ghcCommands x

    gmPutStr res >> gmPutStrLn "OK" >> liftIO (hFlush stdout)
    legacyInteractiveLoop symdbreq world'
 where
    interactiveHandlers =
          [ GHandler $ \e@(FatalError _) -> throw e
          , GHandler $ \e@(ExitSuccess) -> throw e
          , GHandler $ \e@(ExitFailure _) -> throw e
          , GHandler $ \(InvalidCommandLine (Right e)) -> gmErrStrLn e >> return ""
          , GHandler $ \(SomeException e) -> gmErrStrLn (show e) >> return ""
          ]

getFileSourceFromStdin :: IO String
getFileSourceFromStdin = do
  linesIn <- readStdin'
  return (intercalate "\n" linesIn)
  where
    readStdin' = do
      x <- getLine
      if x/="\EOT"
        then fmap (x:) readStdin'
        else return []

-- Someone please already rewrite the cmdline parsing code *weep* :'(
wrapGhcCommands :: (IOish m, GmOut m) => Options -> GhcModCommands -> m ()
wrapGhcCommands _opts CmdRoot = gmPutStr =<< rootInfo
wrapGhcCommands opts cmd = do
    handleGmError $ runGhcModT opts $ handler $ do
      forM_ (reverse $ optFileMappings opts) $
        uncurry loadMMappedFiles

      gmPutStr =<< ghcCommands cmd
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


ghcCommands :: IOish m => GhcModCommands -> GhcModT m String
-- ghcCommands cmd = action args
ghcCommands (CmdLang) = languages
ghcCommands (CmdFlag) = flags
ghcCommands (CmdDebug) = debugInfo
ghcCommands (CmdDebugComponent ts) = componentInfo ts
ghcCommands (CmdBoot) = boot
-- ghcCommands (CmdNukeCaches) = nukeCaches >> return ""
-- ghcCommands (CmdRoot) = undefined -- handled in wrapGhcCommands
ghcCommands (CmdLegacyInteractive) = legacyInteractive >> return ""
ghcCommands (CmdModules detail) = modules detail
ghcCommands (CmdDumpSym tmpdir) = dumpSymbol tmpdir
ghcCommands (CmdFind symb) = findSymbol symb
ghcCommands (CmdDoc m) = pkgDoc m
ghcCommands (CmdLint opts file) = lint opts file
ghcCommands (CmdBrowse opts ms) = concat <$> browse opts `mapM` ms
ghcCommands (CmdCheck files) = checkSyntax files
ghcCommands (CmdExpand files) = expandTemplate files
ghcCommands (CmdInfo file symb) = info file $ Expression symb
ghcCommands (CmdType file (line, col)) = types file line col
ghcCommands (CmdSplit file (line, col)) = splits file line col
ghcCommands (CmdSig file (line, col)) = sig file line col
ghcCommands (CmdAuto file (line, col)) = auto file line col
ghcCommands (CmdRefine file (line, col) expr) = refine file line col $ Expression expr
ghcCommands _ = fatalError "Unknown command"

newtype FatalError = FatalError String deriving (Show, Typeable)
instance Exception FatalError

newtype InvalidCommandLine = InvalidCommandLine (Either String String)
    deriving (Show, Typeable)
instance Exception InvalidCommandLine

exitError :: (MonadIO m, GmOut m) => String -> m a
exitError msg = gmErrStrLn (dropWhileEnd (=='\n') msg) >> liftIO exitFailure

fatalError :: String -> a
fatalError s = throw $ FatalError $ "ghc-mod: " ++ s

catchArgs :: (Monad m, ExceptionMonad m) => String -> m a -> m a
catchArgs cmd action =
    action `gcatch` \(PatternMatchFail _) ->
        throw $ InvalidCommandLine (Left cmd)

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
