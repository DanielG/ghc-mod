-- ghc-mod: Making Haskell development *more* fun
-- Copyright (C) 2016  Daniel Gröber <dxld@darkboxed.org>
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

{-# LANGUAGE RecordWildCards, ViewPatterns, DeriveGeneric, DeriveAnyClass #-}

module Main where

import Data.IORef
import Data.List
import Data.List.Split
import Data.Serialize

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception

import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Daemon
import System.Process
import System.Process.Concurrent
import System.Environment

import GHC.Generics

main = do
  args <- getArgs
  print args
  st <- start args
  print st
  print =<< (waitForCProcess $ gmhProcess $ dsHandle st)

--  writeChan (gmhInChan $ dsHandle st) ""
  print st

--  stop (dsHandle st)

_main = do
  let dopts = DaemonOptions 12351 InHome False

  mv <- newMVar Nothing
  ensureDaemonRunning "ghc-mod-vim" dopts (daemonHander mv)

  args <- getArgs

  res <- runClient "localhost" 12351 args

  print (res :: Maybe Response)

  return ()
  -- case res of
  --   Nothing -> error "ghc-mod-vim: could not connect to daemon"
  --   Just (Left err) -> handleParseResult (Failure err)
  --   Just (Right s)  -> putStr s

data DaemonState = DaemonState {
      dsOptions :: [String]
    , dsHandle  :: GhcModHandle
    } deriving (Show)

data GhcModHandle = GhcModHandle {
      gmhStdin   :: Handle
    , gmhStdout  :: Handle
    , gmhStderr  :: Handle
    , gmhProcess :: CProcessHandle

    , gmhInThd   :: ThreadId
    , gmhOutThd  :: ThreadId
    , gmhErrThd  :: ThreadId

    , gmhInChan  :: Chan String
    , gmhOutChan :: Chan String
    , gmhErrChan :: Chan String

    }

instance Show GhcModHandle where
    show _ = "GhcModHandle {}"


data Request = CheckOptionsReq [String]
             | RunReq [String]
              deriving (Generic, Serialize, Read, Show)

data Response = OKRes [String] | NGRes String | NopRes
              deriving (Generic, Serialize, Read, Show)


daemonHander :: MVar (Maybe DaemonState) -> [String] -> IO Response
daemonHander mv args = do
  let argopts = parseOpts args
      (unArgOpts -> globalOpts, (Arg cmd):rest0) = span isOpt argopts
      cmdArgs = filter isArg rest0
      cmdOpts = filter isOpt rest0


  mstate0 <- takeMVar mv

  state1 <-
      case mstate0 of
        Just (DaemonState globalOpts' gmh)
            | globalOpts /= globalOpts' -> do
                stop gmh
                start globalOpts
        Nothing -> start globalOpts

  res <- commandHandler state1 $ unArgOpts $ cmdArgs ++ cmdOpts
  putMVar mv (Just state1)
  return res

commandHandler :: DaemonState -> [String] -> IO Response
commandHandler state [] = return $ NGRes "no args!?"
commandHandler state args = do
  res <- transaction (dsHandle state) (intercalate " " args)
  case res of
    Left err ->
        return $ NGRes err
    Right a ->
        return $ OKRes a


start args = do
  let cp = (proc "ghc-mod" $ args ++ ["legacy-interactive"]) {
--             close_fds = True,
             std_in  = CreatePipe,
             std_out = CreatePipe,
             std_err = CreatePipe
           }
  (Just hin, Just hout, Just herr, ph) <- createCProcess cp

  ichan <- newChan
  ochan <- newChan
  echan <- newChan

  tin  <- flip forkFinally (\e -> print ("tin", e :: Either SomeException ()))  $ writerThread hin  ichan
  tout <- flip forkFinally (\e -> print ("tout", e :: Either SomeException ())) $ readerThread hout ochan
  terr <- flip forkFinally (\e -> print ("terr", e :: Either SomeException ())) $ readerThread herr echan

  return $ DaemonState args $ GhcModHandle hin hout herr ph tin tout terr ichan ochan echan

writerThread h c = forever $ do
  cmd <- readChan c
  print ("write", cmd)
--  hPutStrLn h cmd

readerThread h c = forever $ do
  l <- hGetLine h
  print ("read", l)
  writeChan c l

stop h@GhcModHandle {..} = do
  mv <- newEmptyMVar

  _ <- forkIO $ do
    send h "" -- empty line terminates legacy-interactive
    waitForCProcess gmhProcess
    putMVar mv ()

  _ <- forkIO $ do
    threadDelay $ 4 * 1000 * 1000 -- wait 4sec before terminating forcefully
    terminateCProcess gmhProcess

  takeMVar mv

send GhcModHandle {..} str =
  writeChan gmhInChan str

recv GhcModHandle {..} =
  readChan gmhOutChan

transaction gmh str = do
  send gmh str
  go []
 where
   go acc = do
     l <- recv gmh
     case l of
       "OK" ->
           return $ Right $ reverse acc
       err@('N':'G':_) ->
           return $ Left err
       _ ->
           go (l : acc)

data ArgOpt = Arg String | Opt [String]
                deriving (Eq, Ord, Read, Show)

isArg (Arg _) = True
isArg _       = False

isOpt (Opt _) = True
isOpt _       = False

unArgOpts :: [ArgOpt] -> [String]
unArgOpts (Arg a  : xs) = a : unArgOpts xs
unArgOpts (Opt as : xs) = as ++ unArgOpts xs

data ParseState = LongOpt String | ShortOpt String
                deriving (Eq, Ord, Read, Show)

parseOpts :: [String] -> [ArgOpt]
parseOpts args = gogo [] args
 where
   gogo acc (x:xs)
       | "--" `isPrefixOf` x =
           go acc (LongOpt x)  xs
       | "-"  `isPrefixOf` x =
           go acc (ShortOpt x) xs
       | otherwise           =
           gogo (Arg x : acc) xs
   gogo acc [] = acc

   go :: [ArgOpt] -> ParseState -> [String] -> [ArgOpt]
   go acc (LongOpt y) (x:xs)
       | '=' `elem` y           =
           gogo (Opt (splitOn "=" y) : acc) (x:xs)
       | y `elem` multiLongOpts =
           gogo (Opt [y,x]           : acc) xs
       | otherwise              =
           gogo (Opt [y]             : acc) (x:xs)

   go acc (ShortOpt ('-':ss)) as
       | [] <- ss =
           gogo acc as
       | [y] <- ss,    y `elem` multiShortOpts, (x:xs) <- as =
           gogo (Opt ['-':[y], x]    : acc) xs
       | (y:ys) <- ss, y `elem` multiShortOpts =
           gogo (Opt ['-':[y], ys]   : acc) as
       | (y:ys) <- ss                          =
           go   (Opt ['-':[y]]       : acc) (ShortOpt ('-':ys)) as

   go acc x _ =
       error $ "expecting more arguments: " ++ show x ++ ", " ++ show acc

multiShortOpts = ['b', 'g']
multiLongOpts  =
    [ "--verbose", "--line-prefix"
    , "--boundary", "--line-seperator"
    , "--with-ghc", "--with-ghc-pkg", "--with-cabal", "--with-stack"
    , "--ghc-option", "--ghcOpt"
    , "--file-mapping"
    , "--encoding"
    ]
