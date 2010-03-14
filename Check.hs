module Check (checkSyntax) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Param
import System.Directory
import System.FilePath
import System.IO
import System.Process

----------------------------------------------------------------

checkSyntax :: Options -> String -> IO String
checkSyntax opt file = do
    makeDirectory (outDir opt)
    (_,_,herr,_) <- runInteractiveProcess (ghc opt) ["--make","-Wall",file,"-outputdir","dist/flymake","-o","dist/flymake/a.out"] Nothing Nothing
    refine <$> hGetContents herr
  where
    refine = unfoldLines . map shrinkSpaces . remove . lines
    remove = filter (\x -> not ("Linking" `isPrefixOf` x))
           . filter (\x -> not ("[" `isPrefixOf` x))
           . filter (/="")
    shrinkSpaces x
      | isSpace (head x) = ' ' : dropWhile isSpace x
      | otherwise        = x

unfoldLines :: [String] -> String
unfoldLines [] = ""
unfoldLines (x:xs) = x ++ unfold xs
  where
    unfold [] = "\n"
    unfold (l:ls)
      | isAlpha (head l)           = ('\n':l) ++ unfold ls
      | " Inferred" `isPrefixOf` l = "." ++ l ++ unfold ls
      | otherwise                  = l ++ unfold ls

----------------------------------------------------------------

makeDirectory :: FilePath -> IO ()
makeDirectory dir = makeDirectoryRecur $ normalise dir
  where
    makeDirectoryRecur "" = return ()
    makeDirectoryRecur cur = do
      exist <- doesDirectoryExist cur
      let par = takeDirectory cur
      unless exist $ do
          makeDirectoryRecur par
          createDirectory cur
