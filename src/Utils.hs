module Utils where

-- |
--
-- >>> split "foo bar baz"
-- ["foo","bar baz"]
-- >>> split "foo  bar  baz"
-- ["foo","bar  baz"]
split :: String -> [String]
split xs = [ys, dropWhile isSpace zs]
  where
    isSpace = (== ' ')
    (ys,zs) = break isSpace xs

-- |
--
-- >>> splitN 0 "foo  bar  baz"
-- ["foo","bar  baz"]
-- >>> splitN 2 "foo  bar  baz"
-- ["foo","bar  baz"]
-- >>> splitN 3 "foo  bar  baz"
-- ["foo","bar","baz"]
splitN :: Int -> String -> [String]
splitN n xs
  | n <= 2    = split xs
  | otherwise = let [ys,zs] = split xs
                in ys : splitN (n - 1) zs
