module Bar.Baz (baz) where
import Foo (foo)

baz :: String
baz = unwords [foo, "baz"]
