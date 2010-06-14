module Types where

import Exception
import GHC
import GHC.Paths (libdir)

data Options = Options {
    convert   :: [String] -> String
  , hlintOpts :: [String]
  }

withGHC :: Ghc [String] -> IO [String]
withGHC body = ghandle ignore $ runGhc (Just libdir) body
  where
    ignore :: SomeException -> IO [String]
    ignore _ = return []

initSession0 :: Ghc [PackageId]
initSession0 = getSessionDynFlags >>= setSessionDynFlags
