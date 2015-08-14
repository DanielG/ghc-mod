{-# LANGUAGE QuasiQuotes #-}
module QuasiQuotes where

import FooQ

bar = [fooQ| foo bar baz |]
