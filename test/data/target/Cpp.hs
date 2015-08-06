{-# LANGUAGE CPP #-}
#undef NOTHING
#ifdef NOTHING
module WRONG_MODULE where
#else
module Cpp where
#endif
