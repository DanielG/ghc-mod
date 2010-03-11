module List (listModules) where

import Control.Applicative
import Data.Char
import Data.List
import System.IO
import System.Process

----------------------------------------------------------------

listModules :: IO [String]
listModules = exposedModules <$> getDump

getDump :: IO String
getDump = do
  (_,hout,_,_) <- runInteractiveProcess "ghc-pkg" ["dump"] Nothing Nothing
  hGetContents hout

exposedModules :: String -> [String]
exposedModules cs = results
  where
    ls = unfoldLines cs
    ns = values "name: " ls
    ms = values "exposed-modules: " ls
    zs = zip ns ms
    xs = filter (\(nm,_) -> nm `notElem` ["ghc", "ghc-prim", "rts", "integer"]) zs
    ss = map snd xs
    results = filter (\x -> not ("GHC" `isPrefixOf` x)) $ concatMap words ss

values :: String -> [String] -> [String]
values tag ls = value
  where
    value = map (drop len) fs
    len = length tag
    fs = filter (tag `isPrefixOf`) ls

----------------------------------------------------------------

unfoldLines :: String -> [String]
unfoldLines xs = self xs
  where
    splitNL = break (== '\n')
    self "" = []
    self s  = let (l, s') = splitNL s
              in case s' of
                []          -> [l]
                (_:' ':s'') -> cont s'' l
                (_:s'')     -> l : self s''
    cont s a = let (l, s') = splitNL $ dropWhile (== ' ') s
                   a' = a ++ " " ++ l
               in case s' of
                 []          -> [a']
                 (_:' ':s'') -> cont s'' a'
                 (_:s'')     -> a' : self s''
