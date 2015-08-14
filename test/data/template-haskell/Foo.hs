module Foo (foo, fooQ) where
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

foo :: ExpQ
foo = stringE "foo"

fooQ :: QuasiQuoter
fooQ = QuasiQuoter (litE . stringL) undefined undefined undefined
