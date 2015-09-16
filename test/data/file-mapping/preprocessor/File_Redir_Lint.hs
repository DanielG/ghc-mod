{-# LANGUAGE CPP #-}
#ifndef NOTHING
module File where

func :: Num a => a -> a -> a
func a b = (*) a b
#else
INVALID
#endif
