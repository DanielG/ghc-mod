-- Haddock01.hs

module Haddock01 where

import Data.Maybe
import qualified Data.List as DL
import qualified Data.Map as DM
import qualified Safe
import qualified Data.List as Foo.Bar

f :: a -> Maybe a
f x = Just x

g :: IO ()
g = do
    let (Just _, _) = (Just 3, Just 4)

    return ()

s = "boo" :: String
s' = head s
t = Just 100 :: Maybe Int
r = DL.length [1, 2, 3]

main = print "Hello, World!"

h = DM.fromList [("x", "y")]

sh = Safe.headMay []

i = 3 :: Int
i' = 3 :: Integer

len = Foo.Bar.length
