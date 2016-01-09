{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Typeable (Typeable)
import Data.List
import Data.List.Split
import Exception
import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Internal hiding (MonadIO,liftIO)
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Find (AsyncSymbolDb, newAsyncSymbolDb, getAsyncSymbolDb)
import System.FilePath ((</>))
import System.Directory (setCurrentDirectory, getAppUserDataDirectory,
                        removeDirectoryRecursive)
import System.IO
import System.Exit
import Text.PrettyPrint hiding ((<>))
import GHCMod.Options
import Prelude

ghcModStyle :: Style
ghcModStyle = style { lineLength = 80, ribbonsPerLine = 1.2 }

----------------------------------------------------------------

handler :: IOish m => GhcModT m a -> GhcModT m a
handler = flip gcatches
          [ GHandler $ \(e :: ExitCode) -> throw e
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
    prepareCabalHelper
    asyncSymbolDb <- newAsyncSymbolDb
    world <- getCurrentWorld
    legacyInteractiveLoop asyncSymbolDb world

legacyInteractiveLoop :: IOish m => AsyncSymbolDb -> World -> GhcModT m ()
legacyInteractiveLoop asyncSymbolDb world = do
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
            lookupSymbol symbol =<< getAsyncSymbolDb asyncSymbolDb
        -- other commands are handled here
        x              -> ghcCommands x

    gmPutStr res >> gmPutStrLn "OK" >> liftIO (hFlush stdout)
    legacyInteractiveLoop asyncSymbolDb world'
 where
    interactiveHandlers =
          [ GHandler $ \(e :: ExitCode) -> throw e
          , GHandler $ \(InvalidCommandLine e) -> do
              let err = notGood $ either ("Invalid command line: "++) Prelude.id e
              liftIO $ do
                putStr err
                exitFailure
          , GHandler $ \(SomeException e) -> gmErrStrLn (show e) >> return ""
          ]
    notGood msg = "NG " ++ escapeNewlines msg
    escapeNewlines = replace "\n" "\\n" . replace "\\n" "\\\\n"
    replace needle replacement = intercalate replacement . splitOn needle

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
wrapGhcCommands opts cmd =
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
ghcCommands (CmdDumpSym) = dumpSymbol >> return ""
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
-- interactive-only commands
ghcCommands (CmdMapFile f) =
      liftIO getFileSourceFromStdin
  >>= loadMappedFileSource f
  >>  return ""
ghcCommands (CmdUnmapFile f) = unloadMappedFile f >> return ""
ghcCommands (CmdQuit) = liftIO exitSuccess
ghcCommands (CmdTest file) = test file
ghcCommands cmd = throw $ InvalidCommandLine $ Left $ show cmd

newtype InvalidCommandLine = InvalidCommandLine (Either String String)
    deriving (Show, Typeable)
instance Exception InvalidCommandLine

exitError :: (MonadIO m, GmOut m) => String -> m a
exitError msg = gmErrStrLn (dropWhileEnd (=='\n') msg) >> liftIO exitFailure

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
