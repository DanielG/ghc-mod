{-# LANGUAGE CPP, FlexibleInstances, FlexibleContexts #-}

module GhcMod.Convert (convert, convert', emptyResult, whenFound, whenFound') where

import GhcMod.Monad.Types
import GhcMod.Types

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
convert' x = flip convert x . optOutput <$> options

convert :: ToString a => OutputOpts -> a -> String
convert opt@OutputOpts { ooptStyle = LispStyle  } x = toLisp opt x "\n"
convert opt@OutputOpts { ooptStyle = PlainStyle } x
  | str == "\n" = ""
  | otherwise   = str
  where
    str = toPlain opt x "\n"

class ToString a where
  toLisp  :: OutputOpts -> a -> Builder
  toPlain :: OutputOpts -> a -> Builder

lineSep :: OutputOpts -> String
lineSep oopts = interpret lsep
  where
    interpret s = read $ "\"" ++ s ++ "\""
    LineSeparator lsep = ooptLineSeparator oopts

-- |
--
-- >>> toLisp (optOutput defaultOptions) "fo\"o" ""
-- "\"fo\\\"o\""
-- >>> toPlain (optOutput defaultOptions) "foo" ""
-- "foo"
instance ToString String where
  toLisp  oopts = quote oopts
  toPlain oopts = replace '\n' (lineSep oopts)

-- |
--
-- >>> toLisp (optOutput defaultOptions) ["foo", "bar", "ba\"z"] ""
-- "(\"foo\" \"bar\" \"ba\\\"z\")"
-- >>> toPlain (optOutput defaultOptions) ["foo", "bar", "baz"] ""
-- "foo\nbar\nbaz"
instance ToString [String] where
  toLisp  oopts = toSexp1 oopts
  toPlain oopts = inter '\n' . map (toPlain oopts)

instance ToString [ModuleString] where
  toLisp  oopts = toLisp oopts . map getModuleString
  toPlain oopts = toPlain oopts . map getModuleString

-- |
--
-- >>> let inp = [((1,2,3,4),"foo"),((5,6,7,8),"bar")] :: [((Int,Int,Int,Int),String)]
-- >>> toLisp (optOutput defaultOptions) inp ""
-- "((1 2 3 4 \"foo\") (5 6 7 8 \"bar\"))"
-- >>> toPlain (optOutput defaultOptions) inp ""
-- "1 2 3 4 \"foo\"\n5 6 7 8 \"bar\""
instance ToString [((Int,Int,Int,Int),String)] where
  toLisp  oopts = toSexp2 . map toS
    where
      toS x = ('(' :) . tupToString oopts x . (')' :)
  toPlain oopts = inter '\n' . map (tupToString oopts)

instance ToString ((Int,Int,Int,Int),String) where
  toLisp  oopts x = ('(' :) . tupToString oopts x . (')' :)
  toPlain oopts x = tupToString oopts x

instance ToString ((Int,Int,Int,Int),[String]) where
  toLisp  oopts (x,s) = ('(' :) .  fourIntsToString x .
                        (' ' :) . toLisp oopts s . (')' :)
  toPlain oopts (x,s) = fourIntsToString x . ('\n' :) . toPlain oopts s

instance ToString (String, (Int,Int,Int,Int),[String]) where
  toLisp  oopts (s,x,y) = toSexp2 [toLisp oopts s, ('(' :) . fourIntsToString x . (')' :), toLisp oopts y]
  toPlain oopts (s,x,y) = inter '\n' [toPlain oopts s, fourIntsToString x, toPlain oopts y]

toSexp1 :: OutputOpts -> [String] -> Builder
toSexp1 oopts ss = ('(' :) . inter ' ' (map (quote oopts) ss) . (')' :)

toSexp2 :: [Builder] -> Builder
toSexp2 ss = ('(' :) . inter ' ' ss . (')' :)

fourIntsToString :: (Int,Int,Int,Int) -> Builder
fourIntsToString (a,b,c,d) = (show a ++) . (' ' :)
                           . (show b ++) . (' ' :)
                           . (show c ++) . (' ' :)
                           . (show d ++)

tupToString :: OutputOpts -> ((Int,Int,Int,Int),String) -> Builder
tupToString oopts ((a,b,c,d),s) = (show a ++) . (' ' :)
                                . (show b ++) . (' ' :)
                                . (show c ++) . (' ' :)
                                . (show d ++) . (' ' :)
                                . quote oopts s -- fixme: quote is not necessary

quote :: OutputOpts -> String -> Builder
quote oopts str = ("\"" ++) .  (quote' str ++) . ("\"" ++)
  where
    lsep = lineSep oopts
    quote' [] = []
    quote' (x:xs)
      | x == '\n' = lsep   ++ quote' xs
      | x == '\\' = "\\\\" ++ quote' xs
      | x == '"'  = "\\\"" ++ quote' xs
      | otherwise = x       : quote' xs

----------------------------------------------------------------

-- Empty result to be returned when no info can be gathered
emptyResult :: Monad m => OutputOpts -> m String
emptyResult oopts = return $ convert oopts ([] :: [String])

-- Return an emptyResult when Nothing
whenFound :: (Monad m, ToString b) => OutputOpts -> m (Maybe a) -> (a -> b) -> m String
whenFound oopts from f = maybe (emptyResult oopts) (return . convert oopts . f) =<< from

-- Return an emptyResult when Nothing, inside a monad
whenFound' :: (Monad m, ToString b) => OutputOpts -> m (Maybe a) -> (a -> m b) -> m String
whenFound' oopts from f = maybe (emptyResult oopts) (\x -> do y <- f x ; return (convert oopts y)) =<< from
