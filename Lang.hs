module Lang where

import Control.Applicative
import Param
import System.IO
import System.Process

listLanguages :: Options -> IO String
listLanguages opt = convert opt . lines <$> getLangs opt

getLangs :: Options -> IO String
getLangs opt = do
  (_,hout,_,_) <- runInteractiveProcess (ghc opt) ["--supported-languages"] Nothing Nothing
  hGetContents hout
