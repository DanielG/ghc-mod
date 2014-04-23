module Main where

import Control.DeepSeq (NFData)
import Data.Complex (Complex((:+)))
import Data.Function (on)

test1 :: Int
test1 = undefined

test2 :: a -> a -> Complex a
test2 = (:+)

test25 :: NFData a => a
test25 = undefined

test3 :: (b -> b -> c) -> (a -> b) -> a -> a -> c
test3 = on

test4 :: IO ()
test4 = putStrLn "Bar"

test5 :: [t] -> ()
test5 (_:_) = ()
test5 _ = error "test5"

-- hlint
test6 :: [Integer] -> [Integer]
test6 = map ((+ 1) . (* 2))
