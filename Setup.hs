#!/usr/bin/env runhaskell

import Distribution.Simple
import Distribution.Simple.Program

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
   hookedPrograms = [ simpleProgram "shelltest" ]
 }
