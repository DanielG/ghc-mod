{-# LANGUAGE QuasiQuotes #-}
module Baz (baz) where
import Foo (fooQ)

baz = [fooQ| foo bar baz |]
