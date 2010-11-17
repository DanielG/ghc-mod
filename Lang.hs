{-# LANGUAGE CPP #-}

module Lang where

import DynFlags
import Types

listLanguages :: Options -> IO String
#if __GLASGOW_HASKELL__ >= 700
listLanguages opt = return $ convert opt supportedLanguagesAndExtensions
#else
listLanguages opt = return $ convert opt supportedLanguages
#endif
