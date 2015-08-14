{-# LANGUAGE CPP, FlexibleInstances, FlexibleContexts #-}

module Language.Haskell.GhcMod.Convert (convert, convert', emptyResult, whenFound, whenFound') where

import Language.Haskell.GhcMod.Monad.Types
import Language.Haskell.GhcMod.Types

import Control.Applicative
import Prelude

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

convert' :: (ToString a, IOish m, GmEnv m) => a -> m String
convert' x = flip convert x <$> options

convert :: ToString a => Options -> a -> String
convert opt@Options { outputStyle = LispStyle  } x = toLisp opt x "\n"
convert opt@Options { outputStyle = PlainStyle } x
  | str == "\n" = ""
  | otherwise   = str
  where
    str = toPlain opt x "\n"

class ToString a where
  toLisp  :: Options -> a -> Builder
  toPlain :: Options -> a -> Builder

lineSep :: Options -> String
lineSep opt = interpret lsep
  where
    interpret s = read $ "\"" ++ s ++ "\""
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

instance ToString [ModuleString] where
  toLisp  opt = toLisp opt . map getModuleString
  toPlain opt = toPlain opt . map getModuleString

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
  toLisp  opt (x,s) = ('(' :) . fourIntsToString opt x .
                      (' ' :) . toLisp opt s . (')' :)
  toPlain opt (x,s) = fourIntsToString opt x . ('\n' :) . toPlain opt s

instance ToString (String, (Int,Int,Int,Int),[String]) where
  toLisp  opt (s,x,y) = toSexp2 [toLisp opt s, ('(' :) . fourIntsToString opt x . (')' :), toLisp opt y]
  toPlain opt (s,x,y) = inter '\n' [toPlain opt s, fourIntsToString opt x, toPlain opt y]

toSexp1 :: Options -> [String] -> Builder
toSexp1 opt ss = ('(' :) . inter ' ' (map (quote opt) ss) . (')' :)

toSexp2 :: [Builder] -> Builder
toSexp2 ss = ('(' :) . inter ' ' ss . (')' :)

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

-- Empty result to be returned when no info can be gathered
emptyResult :: Monad m => Options -> m String
emptyResult opt = return $ convert opt ([] :: [String])

-- Return an emptyResult when Nothing
whenFound :: (Monad m, ToString b) => Options -> m (Maybe a) -> (a -> b) -> m String
whenFound opt from f = maybe (emptyResult opt) (return . convert opt . f) =<< from

-- Return an emptyResult when Nothing, inside a monad
whenFound' :: (Monad m, ToString b) => Options -> m (Maybe a) -> (a -> m b) -> m String
whenFound' opt from f = maybe (emptyResult opt) (\x -> do y <- f x ; return (convert opt y)) =<< from
