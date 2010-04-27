module Lang where

import Param
import DynFlags

listLanguages :: Options -> IO String
listLanguages opt = return $ convert opt supportedLanguages
