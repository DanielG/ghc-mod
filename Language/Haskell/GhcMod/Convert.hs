{-# LANGUAGE FlexibleInstances, FlexibleContexts, OverlappingInstances #-}

module Language.Haskell.GhcMod.Convert (convert, convert') where

import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Types

import Control.Applicative ((<$>))

type Builder = String -> String

-- |
--
-- >>> replace '"' "\\\"" "foo\"bar" ""
-- "foo\\\"bar"
replace :: Char -> String -> String -> Builder
replace _ _  [] = id
replace c cs (x:xs)
  | x == c    = (cs ++) . replace c cs xs
  | otherwise = (x :) . replace c cs xs

inter :: Char -> [Builder] -> Builder
inter _ [] = id
inter c bs = foldr1 (\x y -> x . (c:) . y) bs

convert' :: ToString a => a -> GhcMod String
convert' x = flip convert x <$> options

convert :: ToString a => Options -> a -> String
convert opt@Options { outputStyle = LispStyle  } x = toLisp  opt x "\n"
convert opt@Options { outputStyle = PlainStyle } x
  | str == "\n" = ""
  | otherwise   = str
  where
    str = toPlain opt x "\n"

class ToString a where
    toLisp  :: Options -> a -> Builder
    toPlain :: Options -> a -> Builder

lineSep :: Options -> String
lineSep opt = lsep
  where
    LineSeparator lsep = lineSeparator opt

-- |
--
-- >>> toLisp defaultOptions "fo\"o" ""
-- "\"fo\\\"o\""
-- >>> toPlain defaultOptions "foo" ""
-- "foo"
instance ToString String where
    toLisp  opt = quote opt
    toPlain opt = replace '\n' (lineSep opt)

-- |
--
-- >>> toLisp defaultOptions ["foo", "bar", "ba\"z"] ""
-- "(\"foo\" \"bar\" \"ba\\\"z\")"
-- >>> toPlain defaultOptions ["foo", "bar", "baz"] ""
-- "foo\nbar\nbaz"
instance ToString [String] where
    toLisp  opt = toSexp1 opt
    toPlain opt = inter '\n' . map (toPlain opt)

-- |
--
-- >>> let inp = [((1,2,3,4),"foo"),((5,6,7,8),"bar")] :: [((Int,Int,Int,Int),String)]
-- >>> toLisp defaultOptions inp ""
-- "((1 2 3 4 \"foo\") (5 6 7 8 \"bar\"))"
-- >>> toPlain defaultOptions inp ""
-- "1 2 3 4 \"foo\"\n5 6 7 8 \"bar\""
instance ToString [((Int,Int,Int,Int),String)] where
    toLisp  opt = toSexp2 . map toS
      where
        toS x = ('(' :) . tupToString opt x . (')' :)
    toPlain opt = inter '\n' . map (tupToString opt)

instance ToString ((Int,Int,Int,Int),String) where
    toLisp  opt x = ('(' :) . tupToString opt x . (')' :)
    toPlain opt x = tupToString opt x

instance ToString ((Int,Int,Int,Int),[String]) where
    toLisp  opt (x,y) = toSexp2 $ [('(' :) . fourIntsToString opt x . (')' :), toLisp opt y]
    toPlain opt (x,y) = inter '\n' [fourIntsToString opt x, toPlain opt y]

instance ToString [(Int,Int,Int,Int)] where
    toLisp  opt = toSexp2 . map toS
      where
        toS x = ('(' :) . fourIntsToString opt x . (')' :)
    toPlain opt = inter '\n' . map (fourIntsToString opt)

instance (ToString a, ToString b) => ToString (a,b) where
    toLisp  opt (x,y) = toSexp2 $ [toLisp opt x, toLisp opt y]
    toPlain opt (x,y) = inter '\n' [toPlain opt x, toPlain opt y]

instance (ToString a, ToString b, ToString c) => ToString (a,b,c) where
    toLisp  opt (x,y,z) = toSexp2 $ [toLisp opt x, toLisp opt y, toLisp opt z]
    toPlain opt (x,y,z) = inter '\n' [toPlain opt x, toPlain opt y, toPlain opt z]

instance (ToString a, ToString b, ToString c, ToString d) => ToString (a,b,c,d) where
    toLisp  opt (x,y,z,t) = toSexp2 $ [toLisp opt x, toLisp opt y, toLisp opt z, toLisp opt t]
    toPlain opt (x,y,z,t) = inter '\n' [toPlain opt x, toPlain opt y, toPlain opt z, toPlain opt t]

toSexp1 :: Options -> [String] -> Builder
toSexp1 opt ss = ('(' :) . inter ' ' (map (quote opt) ss) . (')' :)

toSexp2 :: [Builder] -> Builder
toSexp2 ss = ('(' :) . (inter ' ' ss) . (')' :)

fourIntsToString :: Options -> (Int,Int,Int,Int) -> Builder
fourIntsToString _ (a,b,c,d) = (show a ++) . (' ' :)
                             . (show b ++) . (' ' :)
                             . (show c ++) . (' ' :)
                             . (show d ++)

tupToString :: Options -> ((Int,Int,Int,Int),String) -> Builder
tupToString opt ((a,b,c,d),s) = (show a ++) . (' ' :)
                              . (show b ++) . (' ' :)
                              . (show c ++) . (' ' :)
                              . (show d ++) . (' ' :)
                              . quote opt s -- fixme: quote is not necessary

quote :: Options -> String -> Builder
quote opt str = ("\"" ++) .  (quote' str ++) . ("\"" ++)
  where
    lsep = lineSep opt
    quote' [] = []
    quote' (x:xs)
      | x == '\n' = lsep   ++ quote' xs
      | x == '\\' = "\\\\" ++ quote' xs
      | x == '"'  = "\\\"" ++ quote' xs
      | otherwise = x       : quote' xs

----------------------------------------------------------------
