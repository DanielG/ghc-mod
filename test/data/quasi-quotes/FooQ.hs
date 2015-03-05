module FooQ (fooQ) where
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

fooQ :: QuasiQuoter
fooQ = QuasiQuoter (litE . stringL) undefined undefined undefined
