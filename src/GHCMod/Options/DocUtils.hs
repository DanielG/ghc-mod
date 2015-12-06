module GHCMod.Options.DocUtils (
  module PP,
  desc,
  code,
  ($$),
  (##),
  (<$$>),
  (<||>)
) where

import Options.Applicative
import Text.PrettyPrint.ANSI.Leijen as PP hiding ((<>), (<$>), (<$$>), int)

desc :: [Doc] -> InfoMod a
desc = footerDoc . Just . indent 2 . vsep

code :: [String] -> Doc
code x = vsep [line, indent 4 $ vsep $ map text x, line]

infixl 7 <||>
infixr 8 <$$>
infixr 8 $$
infixr 9 ##

($$) :: (a -> b) -> a -> b
($$) = ($)

(<||>) :: Alternative a => a b -> a b -> a b
(<||>) = (<|>)

(##) :: Monoid m => m -> m -> m
(##) = (<>)

(<$$>) :: Functor f => (a -> b) -> f a -> f b
(<$$>) = (<$>)
