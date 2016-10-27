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
import Language.Haskell.GhcMod.Outputable
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
main =
    parseArgs >>= \res@(globalOptions, _) -> do
      enc <- mkTextEncoding $ optEncoding globalOptions
      hSetEncoding stdout enc
      hSetEncoding stderr enc
      hSetEncoding stdin enc
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
        x              -> ghcCommands PlainFormat x

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

wrapGhcCommands :: (IOish m, GmOut m) => Options -> GhcModCommands -> m ()
wrapGhcCommands opts cmd =
    handleGmError $ runGhcModT opts $ handler $ do
      forM_ (reverse $ optFileMappings opts) $
        uncurry loadMMappedFiles

      gmPutStr =<< ghcCommands PlainFormat cmd
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


ghcCommands :: IOish m => OutputFormat -> GhcModCommands -> GhcModT m String
-- ghcCommands fmt cmd = action args
ghcCommands fmt (CmdLang) = languages >>= showOutput fmt NoOutputConfig
ghcCommands fmt (CmdFlag) = flags >>= showOutput fmt NoOutputConfig
ghcCommands fmt (CmdDebug) = debugInfo >>= showOutput fmt NoOutputConfig
ghcCommands fmt (CmdDebugComponent ts) = componentInfo ts >>= showOutput fmt NoOutputConfig
ghcCommands fmt (CmdBoot) = boot >>= showOutput fmt NoOutputConfig
-- ghcCommands fmt (CmdNukeCaches) = nukeCaches >> return ""
ghcCommands fmt (CmdRoot) = rootInfo >>= showOutput fmt NoOutputConfig
ghcCommands fmt (CmdLegacyInteractive) = legacyInteractive >> return ""
ghcCommands fmt (CmdModules detail) = modules detail >>= showOutput fmt NoOutputConfig
ghcCommands fmt (CmdDumpSym) = dumpSymbol >> return ""
ghcCommands fmt (CmdFind symb) = findSymbol symb >>= showOutput fmt NoOutputConfig
ghcCommands fmt (CmdDoc m) = pkgDoc m >>= showOutput fmt NoOutputConfig
ghcCommands fmt (CmdLint opts file) = lint opts file >>= showOutput fmt NoOutputConfig
ghcCommands fmt (CmdBrowse opts ms) = (concat <$> browse opts `mapM` ms) >>= showOutput fmt NoOutputConfig
ghcCommands fmt (CmdCheck files) = checkSyntax files >>= showOutput fmt NoOutputConfig
ghcCommands fmt (CmdExpand files) = expandTemplate files >>= showOutput fmt NoOutputConfig
ghcCommands fmt (CmdInfo file symb) = info file (Expression symb) >>= showOutput fmt NoOutputConfig
ghcCommands fmt (CmdType wCon file (line, col)) = types wCon file line col >>= showOutput fmt NoOutputConfig
ghcCommands fmt (CmdSplit file (line, col)) = splits file line col >>= showOutput fmt NoOutputConfig
ghcCommands fmt (CmdSig file (line, col)) = sig file line col >>= showOutput fmt NoOutputConfig
ghcCommands fmt (CmdAuto file (line, col)) = auto file line col >>= showOutput fmt NoOutputConfig
ghcCommands fmt (CmdRefine file (line, col) expr) = refine file line col (Expression expr) >>= showOutput fmt NoOutputConfig
-- interactive-only commands
ghcCommands _ (CmdMapFile f) =
      liftIO getFileSourceFromStdin
  >>= loadMappedFileSource f
  >>  return ""
ghcCommands _ (CmdUnmapFile f) = unloadMappedFile f >> return ""
ghcCommands _ (CmdQuit) = liftIO exitSuccess
ghcCommands fmt (CmdTest file) = test file >>= showOutput fmt NoOutputConfig
ghcCommands _ cmd = throw $ InvalidCommandLine $ Left $ show cmd

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
