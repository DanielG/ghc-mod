{-# LANGUAGE PatternSynonyms #-}
module B where
import A

foo :: SomeType Int Char -> String
foo x = case x of
          MyPat a b -> show a ++ " " ++ [b]
