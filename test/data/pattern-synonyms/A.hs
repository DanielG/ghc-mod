{-# LANGUAGE PatternSynonyms #-}

module A where

data SomeType a b = SomeType (a,b)

pattern MyPat x y <- SomeType (x,y)
