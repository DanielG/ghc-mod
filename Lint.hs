module Lint where

import Control.Applicative
import Data.List
import System.IO
import System.Process
import Types

lintSyntax :: Options -> String -> IO String
lintSyntax cmd file = pretty <$> lint cmd file
  where
    pretty = unlines . map (concat . intersperse "\0")
           . filter (\x -> length x > 1)
           . groupBy (\a b -> a /= "" && b /= "") 
           . lines

lint :: Options -> String -> IO String
lint cmd file = do
  (_,hout,_,_) <- runInteractiveProcess (hlint cmd) [file] Nothing Nothing
  hGetContents hout
