module CabalHelper.Log where

import Control.Monad
import Control.Monad.IO.Class
import Control.Exception as E
import Data.String
import System.IO
import Prelude

import CabalHelper.Types

vLog :: MonadIO m => Options -> String -> m ()
vLog Options { verbose = True } msg =
    liftIO $ hPutStrLn stderr msg
vLog _ _ = return ()

logSomeError :: Options -> String -> IO (Maybe a) -> IO (Maybe a)
logSomeError opts label a = do
  a `E.catch` \se@(SomeException _) -> do
      vLog opts $ label ++ ": " ++ show se
      return Nothing
