{-# LANGUAGE CPP #-}

#ifndef MIN_VERSION_cabal_doctest
#error \
    Your version of cabal-install does not seem to support the 'custom-setup' section. \
    Please see https://github.com/DanielG/ghc-mod/wiki/Installing#checking-and-installing-prerequisites for instrutions on how to upgrade. \
    It is also possible that you forgot to install cabal-doctest before running Setup.hs
#endif

import Distribution.Simple
import Distribution.Simple.Program

import Distribution.Extra.Doctest

main :: IO ()
main =
    defaultMainWithHooks $
    addDoctestsUserHook "doctest" $
    simpleUserHooks {
        hookedPrograms = [ simpleProgram "shelltest" ]
    }
