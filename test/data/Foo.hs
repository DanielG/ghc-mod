module Foo (foo) where
import Language.Haskell.TH

foo :: ExpQ
foo = stringE "foo"
