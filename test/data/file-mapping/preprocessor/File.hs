{-# LANGUAGE CPP #-}
#ifndef NOTHING
main :: IO ()
main = putStrLn "Hello World!"
#else
INVALID
#endif
