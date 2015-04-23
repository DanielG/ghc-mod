{-# LANGUAGE ForeignFunctionInterface #-}

module ForeignExport where

import Foreign.C.Types

foreign export ccall foo :: CUInt

foo :: CUInt
foo = 123
