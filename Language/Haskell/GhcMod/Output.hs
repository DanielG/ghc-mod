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
  , gmUnsafePutStrLn
  , gmUnsafeErrStrLn
  , gmReadProcess
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

outputFns :: (GmEnv m, MonadIO m')
          => m (GmLines String -> m' (), GmLines String -> m' ())
outputFns = do
  opts <- options
  env <- gmeAsk
  return $ outputFns' opts (gmOutput env)

outputFns' :: MonadIO m'
           => Options
           -> GmOutput
           -> (GmLines String -> m' (), GmLines String -> m' ())
outputFns' opts output  = let
  Options {..} = opts

  pfx f = withLines f

  outPfx, errPfx :: GmLines String -> GmLines String
  (outPfx, errPfx) =
      case linePrefix of
        Nothing -> ( id, id )
        Just (op, ep) -> ( fmap $ pfx (op++), fmap $ pfx (ep++) )
 in
  case output of
    GmOutputStdio  ->
        ( liftIO . putStr         . unGmLine . outPfx
        , liftIO . hPutStr stderr . unGmLine . errPfx)
    GmOutputChan c ->
        ( liftIO . writeChan c . (,) GmOut . outPfx
        , liftIO . writeChan c . (,) GmErr .errPfx)

gmPutStr, gmPutStrLn, gmErrStr, gmErrStrLn
    :: (MonadIO m, GmEnv m) => String -> m ()

gmPutStr str = do
  putOut <- fst `liftM` outputFns
  putOut $ toGmLines str

gmPutStrLn = gmPutStr . (++"\n")
gmErrStrLn = gmErrStr . (++"\n")

gmErrStr str = do
  putErr <- snd `liftM` outputFns
  putErr $ toGmLines str

-- | Only use these when you're sure there are no other writers on stdout
gmUnsafePutStrLn, gmUnsafeErrStrLn
    :: MonadIO m => Options -> String -> m ()
gmUnsafePutStrLn opts = (fst $ outputFns' opts GmOutputStdio) . toGmLines
gmUnsafeErrStrLn opts = (snd $ outputFns' opts GmOutputStdio) . toGmLines

gmReadProcess :: GmEnv m => m (FilePath -> [String] -> String -> IO String)
gmReadProcess = do
  GhcModEnv {..} <- gmeAsk
  case gmOutput of
    GmOutputChan _ ->
        readProcessStderrChan
    GmOutputStdio ->
        return $ readProcess

stdoutGateway :: Chan (GmStream, GmLines String) -> IO ()
stdoutGateway chan = go ("", "")
 where
   go buf@(obuf, ebuf) = do
     (stream, GmLines ty l) <- readChan chan
     case ty of
       GmTerminated ->
           case stream of
             GmOut -> putStr (obuf++l) >> go ("", ebuf)
             GmErr -> putStr (ebuf++l) >> go (obuf, "")
       GmPartial -> case reverse $ lines l of
                      [] -> go buf
                      [x] -> go (appendBuf stream buf x)
                      x:xs -> do
                        putStr $ unlines $ reverse xs
                        go (appendBuf stream buf x)

   appendBuf GmOut (obuf, ebuf) s = (obuf++s, ebuf)
   appendBuf GmErr (obuf, ebuf) s = (obuf, ebuf++s)


readProcessStderrChan ::
    GmEnv m => m (FilePath -> [String] -> String -> IO String)
readProcessStderrChan = do
  (_, e) <- outputFns
  return $ go e
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
           processFailedException "readProcessStderrChan" exe args rv
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

processFailedException :: String -> String -> [String] -> Int -> IO a
processFailedException fn exe args rv =
      error $ concat [ fn, ": ", exe, " "
                     , intercalate " " (map show args)
                     , " (exit " ++ show rv ++ ")"]
