-- ghc-mod: Making Haskell development *more* fun
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

module Language.Haskell.GhcMod.Stderr where

import Data.List
import System.IO
import System.Exit
import System.Process
import Control.Monad
import Control.DeepSeq
import Control.Exception
import Control.Concurrent

stdoutGateway :: Chan String -> IO ()
stdoutGateway chan = do
    l <- readChan chan
    putStrLn l
    stdoutGateway chan

readProcessStderrChan ::
    Chan String -> FilePath -> [String] -> String -> IO String
readProcessStderrChan cout exe args input = do
  let cp = (proc exe args) {
             std_out = CreatePipe
           , std_err = CreatePipe
           , std_in  = CreatePipe
           }
  (Just i, Just o, Just e, h) <- createProcess cp

  _ <- forkIO $ reader e

  output  <- hGetContents o
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
        processFailedException "readProcessStderrChan" exe args rv
    ExitSuccess ->
        return output

 where
   ignoreSEx = handle (\(SomeException _) -> return ())
   reader h = ignoreSEx $ do
     l <- hGetLine h
     writeChan cout l
     reader h

withForkWait :: IO () -> (IO () ->  IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either throwIO return
    restore (body wait) `onException` killThread tid

processFailedException :: String -> String -> [String] -> Int -> IO a
processFailedException fn exe args rv =
      error $ concat [fn, ": ", exe, " "
                     , intercalate " " (map show args)
                     , " (exit " ++ show rv ++ ")"]
