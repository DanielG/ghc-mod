{-# LANGUAGE CPP #-}

module GHCMod.Options.GapUtils (
  readMaybe
) where

#if __GLASGOW_HASKELL__ >= 706
import Text.Read (readMaybe)
#else
readMaybe :: Read a => String -> Maybe a
readMaybe = Just . read
#endif
