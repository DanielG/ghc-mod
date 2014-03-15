
myHead ::  [Int] -> Int
myHead (x:_) = x

main :: IO ()
main = do
    print $ myHead [1,2,3]

