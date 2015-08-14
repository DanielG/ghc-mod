{-# LANGUAGE TemplateHaskell #-}
module Bar (bar) where
import Foo (foo)

bar = $foo ++ "bar"
