module Check (checkSyntax) where

import Control.Applicative
import Data.Char
import Data.List
import System.IO
import System.Process

----------------------------------------------------------------

checkSyntax :: String -> IO String
checkSyntax file = do
    (_,_,herr,_) <- runInteractiveProcess "ghc" ["--make","-Wall",file] Nothing Nothing
    refine <$> hGetContents herr
  where
   refine = unfoldLines start . map (dropWhile isSpace) . filter (/="") . lines
   start = (file `isPrefixOf`)

unfoldLines :: (String -> Bool) -> [String] -> String
unfoldLines p = drop 1 . unfold
  where
    unfold [] = "\n"
    unfold (l:ls)
      | p l       = ('\n':l) ++ unfold ls
      | otherwise = l ++ " " ++ unfold ls
