-- ghc-mod: Happy Haskell Hacking
-- Copyright (C) 2015  Daniel Gröber <dxld ÄT darkboxed DOT org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- Derived from process:System.Process
-- Copyright (c) The University of Glasgow 2004-2008

{-# LANGUAGE FlexibleInstances #-}
module GhcMod.Output (
    gmPutStr
  , gmErrStr
  , gmPutStrLn
  , gmErrStrLn

  , gmPutStrIO
  , gmErrStrIO

  , gmReadProcess
  , gmReadProcess'

  , stdoutGateway
  , flushStdoutGateway
  ) where

import Data.List
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Label as L
import qualified Data.Label.Base as LB
import System.IO
import System.Exit
import System.Process
import Control.Monad
import Control.Monad.State.Strict
import Control.DeepSeq
import Control.Exception
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Pipes
import Pipes.Lift
import Prelude

import GhcMod.Types hiding (LineSeparator, MonadIO(..))
import GhcMod.Monad.Types hiding (MonadIO(..))
import GhcMod.Gap ()

class ProcessOutput a where
  hGetContents' :: Handle -> IO a

instance ProcessOutput String where
  hGetContents' = hGetContents

instance ProcessOutput ByteString where
  hGetContents' = BS.hGetContents

outputFns :: (GmOut m, MonadIO m')
          => m (String -> m' (), String -> m' ())
outputFns =
  outputFns' `liftM` gmoAsk

outputFns' ::
  MonadIO m => GhcModOut -> (String -> m (), String -> m ())
outputFns' (GhcModOut oopts c)  = let
  OutputOpts {..} = oopts
 in
  case ooptLinePrefix of
    Nothing -> stdioOutputFns
    Just _  -> chanOutputFns c

stdioOutputFns :: MonadIO m => (String -> m (), String -> m ())
stdioOutputFns =
    ( liftIO . putStr
    , liftIO . hPutStr stderr
    )

chanOutputFns :: MonadIO m
              => Chan (Either (MVar ()) (GmStream, String))
              -> (String -> m (), String -> m ())
chanOutputFns c = (write GmOutStream, write GmErrStream)
 where
   write stream s = liftIO $ writeChan c $ Right $ (stream,s)

gmPutStr, gmPutStrLn, gmErrStr, gmErrStrLn
    :: (MonadIO m, GmOut m) => String -> m ()

gmPutStr str = do
  putOut <- gmPutStrIO
  putOut str

gmErrStr str = do
  putErr <- gmErrStrIO
  putErr str

gmPutStrLn = gmPutStr . (++"\n")
gmErrStrLn = gmErrStr . (++"\n")

gmPutStrIO, gmErrStrIO :: (GmOut m, MonadIO mi) => m (String -> mi ())

gmPutStrIO = fst `liftM` outputFns
gmErrStrIO = snd `liftM` outputFns


gmReadProcess :: GmOut m => m (FilePath -> [String] -> String -> IO String)
gmReadProcess = do
  GhcModOut {..} <- gmoAsk
  case ooptLinePrefix gmoOptions of
    Just _ ->
        readProcessStderrChan
    Nothing ->
        return $ readProcess

gmReadProcess' :: GmOut m => m (FilePath -> [String] -> String -> IO ByteString)
gmReadProcess' = readProcessStderrChan

flushStdoutGateway :: Chan (Either (MVar ()) (GmStream, String)) -> IO ()
flushStdoutGateway c = do
  mv <- newEmptyMVar
  writeChan c $ Left mv
  takeMVar mv

type Line = String

stdoutGateway :: (String, String) -> Chan (Either (MVar ()) (GmStream, String)) -> IO ()
stdoutGateway (outPf, errPf) chan = do
  runEffect $ commandProc >-> evalStateP ("","") seperateStreams
 where
   commandProc :: Producer (Either (MVar ()) (GmStream, String)) IO ()
   commandProc = do
     cmd <- liftIO $ readChan chan
     case cmd of
       Left mv -> do
           yield $ Left mv
       Right input -> do
           yield $ Right input
           commandProc

   seperateStreams :: Consumer (Either (MVar ()) (GmStream, String)) (StateT (String, String) IO) ()
   seperateStreams  = do
       ecmd <- await
       case ecmd of
         Left mv -> do
           -- flush buffers
           (\s -> lift $ zoom (streamLens s) $ sGetLine Nothing)
               `mapM_` [GmOutStream, GmErrStream]

           liftIO $ putMVar mv ()
         Right (stream, str) -> do
           ls <- lift $ zoom (streamLens stream) $ sGetLine (Just str)
           case ls of
               [] -> return ()
               _  -> liftIO $ putStr $ unlines $ map (streamPf stream++) ls

           liftIO $ hFlush stdout
           seperateStreams

   sGetLine :: (Maybe String) -> StateT String IO [Line]
   sGetLine mstr' = do
     buf <- get
     let mstr = (buf++) `liftM` mstr'
     case mstr of
       Nothing -> put "" >> return [buf]
       Just "" -> return []
       Just s | last s == '\n' -> put "" >> return (lines s)
              | otherwise -> do
                  let (p:ls') = reverse $ lines s
                  put p
                  return $ reverse $ ls'

   streamLens GmOutStream = LB.fst
   streamLens GmErrStream = LB.snd

   streamPf GmOutStream = outPf
   streamPf GmErrStream = errPf

zoom :: Monad m => (f L.:-> o) -> StateT o m a -> StateT f m a
zoom l (StateT a) =
    StateT $ \f -> do
      (a', s') <- a $ L.get l f
      return (a', L.set l s' f)

readProcessStderrChan ::
    (GmOut m, ProcessOutput a, NFData a) => m (FilePath -> [String] -> String -> IO a)
readProcessStderrChan = do
  (_, e :: String -> IO ()) <- outputFns
  return $ readProcessStderrChan' e

readProcessStderrChan' :: (ProcessOutput a, NFData a) =>
    (String -> IO ()) -> FilePath -> [String] -> String -> IO a
readProcessStderrChan' putErr exe args input = do
     let cp = (proc exe args) {
                std_out = CreatePipe
              , std_err = CreatePipe
              , std_in  = CreatePipe
              }
     (Just i, Just o, Just e, h) <- createProcess cp

     _ <- forkIO $ reader e

     output  <- hGetContents' o
     withForkWait (evaluate $ rnf output) $ \waitOut -> do

       -- now write any input
       unless (null input) $
         ignoreSEx $ hPutStr i input
       -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
       ignoreSEx $ hClose i

       -- wait on the output
       waitOut
       hClose o

     res <- waitForProcess h
     case res of
       ExitFailure rv ->
           throw $ GMEProcess "readProcessStderrChan" exe args $ Left rv
       ExitSuccess ->
           return output
    where
      ignoreSEx = handle (\(SomeException _) -> return ())
      reader h = ignoreSEx $ do
        putErr . (++"\n") =<< hGetLine h
        reader h

withForkWait :: IO () -> (IO () ->  IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either throwIO return
    restore (body wait) `onException` killThread tid
