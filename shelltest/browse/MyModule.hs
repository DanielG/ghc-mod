module MyModule where

data MyType = MyConstructor1
            | MyConstructor2

some_num = 0
some_char = ' '
some_string = "bar"

($?>>/) :: Int -> Int -> Int
a $?>>/ b = a + b
