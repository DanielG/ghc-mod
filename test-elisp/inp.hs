module Main where

test1 :: Int

test2 :: a -> a -> Complex a
test2 = (:+)

test25 :: NFData a => a
test25 = undefined

test3 :: (b -> b -> c) -> (a -> b) -> a -> a -> c
test3 = on

test4 = putStrLn "Bar"

test5 :: [t] -> ()
test5 (_:_) = ()

-- hlint
test6 :: [Integer] -> [Integer]
test6 = map (+ 1) . map (* 2)
