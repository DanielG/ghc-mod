module Lang where

import qualified Gap
import Types

listLanguages :: Options -> IO String
listLanguages opt = return $ convert opt Gap.supportedExtensions
