module Main where
import Bar.Baz (baz)

main :: IO ()
main = putStrLn baz
