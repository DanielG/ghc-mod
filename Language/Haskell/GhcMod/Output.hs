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

module Language.Haskell.GhcMod.Output (
    gmPutStr
  , gmErrStr
  , gmPutStrLn
  , gmErrStrLn
  , gmReadProcess
  , gmUnsafePutStr
  , gmUnsafeErrStr
  , stdoutGateway
  ) where

import Data.List
import System.IO
import System.Exit
import System.Process
import Control.Monad
import Control.DeepSeq
import Control.Exception
import Control.Concurrent
import Prelude

import Language.Haskell.GhcMod.Types hiding (LineSeparator)
import Language.Haskell.GhcMod.Monad.Types

withLines :: (String -> String) -> String -> String
withLines f s = let
    res = unlines $ map f $ lines s
  in
    case s of
      [] -> res
      _ | not $ isTerminated s ->
            reverse $ drop 1 $ reverse res
      _ -> res

isTerminated :: String -> Bool
isTerminated "" = False
isTerminated s = isNewline (last s)

isNewline :: Char -> Bool
isNewline c = c == '\n'

toGmLines :: String -> (GmLines String)
toGmLines "" = GmLines GmPartial ""
toGmLines s | isNewline (last s) = GmLines GmTerminated s
toGmLines s = GmLines GmPartial s

outputFns :: (GmOut m, MonadIO m')
          => m (GmLines String -> m' (), GmLines String -> m' ())
outputFns =
  outputFns' `liftM` gmoAsk

pfxFns :: Maybe (String, String) -> (GmLines String -> GmLines String, GmLines String -> GmLines String)
pfxFns lpfx = case lpfx of
    Nothing -> ( id, id )
    Just (op, ep) -> ( fmap $ pfx (op++), fmap $ pfx (ep++) )
  where
    pfx f = withLines f

stdioOutputFns :: MonadIO m => Maybe (String, String) -> (GmLines String -> m (), GmLines String -> m ())
stdioOutputFns lpfx = let
    (outPfx, errPfx) = pfxFns lpfx
  in
    ( liftIO . putStr         . unGmLine . outPfx
    , liftIO . hPutStr stderr . unGmLine . errPfx)

chanOutputFns :: MonadIO m
              => Chan (GmStream, GmLines String)
              -> Maybe (String, String)
              -> (GmLines String -> m (), GmLines String -> m ())
chanOutputFns c lpfx = let
    (outPfx, errPfx) = pfxFns lpfx
  in
    ( liftIO . writeChan c . (,) GmOutStream . outPfx
    , liftIO . writeChan c . (,) GmErrStream . errPfx)

outputFns' ::
  MonadIO m => GhcModOut -> (GmLines String -> m (), GmLines String -> m ())
outputFns' (GhcModOut oopts c)  = let
  OutputOpts {..} = oopts
 in
  case ooptLinePrefix of
    Nothing  -> stdioOutputFns ooptLinePrefix
    Just _ -> chanOutputFns c ooptLinePrefix

gmPutStr, gmPutStrLn, gmErrStr, gmErrStrLn
    :: (MonadIO m, GmOut m) => String -> m ()

gmPutStr str = do
  putOut <- fst `liftM` outputFns
  putOut $ toGmLines str

gmPutStrLn = gmPutStr . (++"\n")
gmErrStrLn = gmErrStr . (++"\n")

gmErrStr str = do
  putErr <- snd `liftM` outputFns
  putErr $ toGmLines str

-- | Only use these when you're sure there are no other writers on stdout
gmUnsafePutStr, gmUnsafeErrStr
    :: MonadIO m => OutputOpts -> String -> m ()
gmUnsafePutStr oopts = (fst $ stdioOutputFns (ooptLinePrefix oopts)) . toGmLines
gmUnsafeErrStr oopts = (snd $ stdioOutputFns (ooptLinePrefix oopts)) . toGmLines

gmReadProcess :: GmOut m => m (FilePath -> [String] -> String -> IO String)
gmReadProcess = do
  GhcModOut {..} <- gmoAsk
  case ooptLinePrefix gmoOptions of
    Just _ ->
        readProcessStderrChan
    Nothing ->
        return $ readProcess

stdoutGateway :: Chan (GmStream, GmLines String) -> IO ()
stdoutGateway chan = go ("", "")
 where
   go buf@(obuf, ebuf) = do
     (stream, GmLines ty l) <- readChan chan
     case ty of
       GmTerminated ->
           case stream of
             GmOutStream -> putStr (obuf++l) >> hFlush stdout >> go ("", ebuf)
             GmErrStream -> putStr (ebuf++l) >> hFlush stdout >> go (obuf, "")
       GmPartial -> case reverse $ lines l of
                      [] -> go buf
                      [x] -> go (appendBuf stream buf x)
                      x:xs -> do
                        putStr $ unlines $ reverse xs
                        hFlush stdout
                        go (appendBuf stream buf x)

   appendBuf GmOutStream (obuf, ebuf) s = (obuf++s, ebuf)
   appendBuf GmErrStream (obuf, ebuf) s = (obuf, ebuf++s)


readProcessStderrChan ::
    GmOut m => m (FilePath -> [String] -> String -> IO String)
readProcessStderrChan = do
  (_, e :: GmLines String -> IO ()) <- outputFns
  return $ readProcessStderrChan' e

readProcessStderrChan' ::
     (GmLines String -> IO ())
  -> FilePath -> [String] -> String -> IO String
readProcessStderrChan' pute = go pute
 where
   go :: (GmLines String -> IO ()) -> FilePath -> [String] -> String -> IO String
   go putErr exe args input = do
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
           throw $ GMEProcess "readProcessStderrChan" exe args $ Left rv
       ExitSuccess ->
           return output
    where
      ignoreSEx = handle (\(SomeException _) -> return ())
      reader h = ignoreSEx $ do
        putErr . toGmLines . (++"\n") =<< hGetLine h
        reader h

withForkWait :: IO () -> (IO () ->  IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either throwIO return
    restore (body wait) `onException` killThread tid
