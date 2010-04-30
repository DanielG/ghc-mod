module Lang where

import DynFlags
import Types

listLanguages :: Options -> IO String
listLanguages opt = return $ convert opt supportedLanguages
