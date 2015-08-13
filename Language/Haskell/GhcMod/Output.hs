module Language.Haskell.GhcMod.Output (
    gmPutStr
  , gmErrStr
  , gmPutStrLn
  , gmErrStrLn
  ) where

import Data.Char
import System.IO
import Control.Monad
import Control.Concurrent

import Language.Haskell.GhcMod.Types hiding (LineSeparator)
import Language.Haskell.GhcMod.Monad.Types

withLines :: (String -> String) -> String -> String
withLines f s = let
    res = unlines $ map f $ lines s
  in
    case s of
      [] -> res
      _ | generalCategory (last s) /= LineSeparator ->
            reverse $ drop 1 $ reverse res
      _ -> res

outputFns :: (GmEnv m, MonadIO m') => m (String -> m' (), String -> m' ())
outputFns = do
  GhcModEnv {..} <- gmeAsk
  let Options {..} = gmOptions

  let pfx f = withLines f
  let (outPfx, errPfx) = case linePrefix of
                           Nothing -> ( id, id )
                           Just (op, ep) -> ( pfx (op++), pfx (ep++) )

  return $ case gmOutput of
    GmOutputStdio  ->
        (liftIO . putStr . outPfx     , liftIO . hPutStr stderr . errPfx)
    GmOutputChan c ->
        (liftIO . writeChan c . outPfx, liftIO . writeChan c . errPfx)

gmPutStr, gmPutStrLn, gmErrStr, gmErrStrLn
    :: (MonadIO m, GmEnv m) => String -> m ()

gmPutStr str = do
  putOut <- fst `liftM` outputFns
  putOut str

gmPutStrLn = gmPutStr . (++"\n")
gmErrStrLn = gmErrStr . (++"\n")

gmErrStr str = do
  putErr <- snd `liftM` outputFns
  putErr str
