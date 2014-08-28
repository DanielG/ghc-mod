module Language.Haskell.GhcMod.Utils where


import Language.Haskell.GhcMod.Error
import MonadUtils (MonadIO, liftIO)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

-- dropWhileEnd is not provided prior to base 4.5.0.0.
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []

extractParens :: String -> String
extractParens str = extractParens' str 0
 where
   extractParens' :: String -> Int -> String
   extractParens' [] _ = []
   extractParens' (s:ss) level
       | s `elem` "([{" = s : extractParens' ss (level+1)
       | level == 0 = extractParens' ss 0
       | s `elem` "}])" && level == 1 = [s]
       | s `elem` "}])" = s : extractParens' ss (level-1)
       | otherwise = s : extractParens' ss level

readProcess' :: (MonadIO m, MonadError GhcModError m)
             => String
             -> [String]
             -> m String
readProcess' cmd opts = do
  (rv,output,err) <- liftIO (readProcessWithExitCode cmd opts "")
      `modifyError'` GMEProcess ([cmd] ++ opts)
  case rv of
    ExitFailure val -> do
        throwError $ GMEProcess ([cmd] ++ opts) $ strMsg $
          cmd ++ " " ++ unwords opts ++ " (exit " ++ show val ++ ")"
              ++ "\n" ++ err
    ExitSuccess ->
        return output

withDirectory_ :: (MonadIO m, ExceptionMonad m) => FilePath -> m a -> m a
withDirectory_ dir action =
    gbracket (liftIO getCurrentDirectory) (liftIO . setCurrentDirectory)
                (\_ -> liftIO (setCurrentDirectory dir) >> action)
