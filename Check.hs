module Check (checkSyntax) where

import Control.Applicative
import Data.Char
import Data.List
import Param
import System.IO
import System.Process

----------------------------------------------------------------

checkSyntax :: Options -> String -> IO String
checkSyntax opt file = do
    (_,_,herr,_) <- runInteractiveProcess (ghc opt) ["--make","-Wall",file] Nothing Nothing
    refine <$> hGetContents herr
  where
   refine = unfoldLines start . map (dropWhile isSpace) . filter (/="") . lines
   start = (file `isPrefixOf`)

unfoldLines :: (String -> Bool) -> [String] -> String
unfoldLines _ [] = ""
unfoldLines p (x:xs) = x ++ unfold xs
  where
    unfold [] = "\n"
    unfold (l:ls)
      | p l       = ('\n':l) ++ unfold ls
      | otherwise = (' ' :l) ++ unfold ls
