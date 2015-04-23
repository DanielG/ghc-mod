module Data.Foo where

foo :: Int
foo = undefined

fibonacci :: Int -> Integer
fibonacci n = fib 1 0 1
  where
    fib m x y
      | n == m    = y
      | otherwise = fib (m+1) y (x + y)
