module List (listModules) where

import Control.Applicative
import Data.List
import Param
import System.IO
import System.Process

----------------------------------------------------------------

listModules :: Options -> IO String
listModules opt = convert opt . nub . sort . exposedModules <$> getDump opt

getDump :: Options -> IO String
getDump opt = do
  (_,hout,_,_) <- runInteractiveProcess (ghcPkg opt) ["dump"] Nothing Nothing
  hGetContents hout

exposedModules :: String -> [String]
exposedModules cs = concatMap words ms
  where
    ls = unfoldLines cs
    ms = values "exposed-modules: " ls

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
