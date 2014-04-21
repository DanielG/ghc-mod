module Language.Haskell.GhcMod.Utils where

-- dropWhileEnd is not provided prior to base 4.5.0.0.
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []

-- |
--
-- >>> replace '"' "\\\"" "foo\"bar"
-- "foo\\\"bar"
replace :: Char -> String -> String -> String
replace _ _  [] = []
replace c cs (x:xs)
  | x == c    = cs ++ replace c cs xs
  | otherwise = x   : replace c cs xs
