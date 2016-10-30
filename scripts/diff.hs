import Data.Tuple
import System.FilePath
import System.Environment
import System.Process

main = do
  vs <- lines <$> getContents
  [pkg, dir] <- getArgs
  mapM_ system $ map (\(v1, v2) -> "diff -u --color=always " ++ file pkg dir v1 ++ " " ++ file pkg dir v2 ++ "; echo; echo; echo") $ map swap $ drop 1 vs `zip` vs
 where
   file pkg dir v = dir </> (pkg ++ "-" ++ v) <.> "cabal"
