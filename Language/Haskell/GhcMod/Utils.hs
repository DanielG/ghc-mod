module Language.Haskell.GhcMod.Utils where

import Control.Exception (bracket)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stderr)

-- dropWhileEnd is not provided prior to base 4.5.0.0.
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []
readProcess' :: String -> [String] -> IO String
readProcess' cmd opts = do
  (rv,output,err) <- readProcessWithExitCode cmd opts ""
  case rv of
    ExitFailure val -> do
        hPutStrLn stderr err
        fail $ cmd ++ " " ++ unwords opts ++ " (exit " ++ show val ++ ")"
    ExitSuccess ->
        return output

withDirectory_ :: FilePath -> IO a -> IO a
withDirectory_ dir action =
    bracket getCurrentDirectory setCurrentDirectory
                (\_ -> setCurrentDirectory dir >> action)
