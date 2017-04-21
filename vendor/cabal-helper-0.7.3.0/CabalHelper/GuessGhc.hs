module CabalHelper.GuessGhc (guessToolFromGhcPath) where

import Data.Maybe
import Data.Char
import Distribution.Simple.BuildPaths
import System.Directory
import System.FilePath

-- Copyright (c) 2003-2014, Isaac Jones, Simon Marlow, Martin Sjögren,
--                          Bjorn Bringert, Krasimir Angelov,
--                          Malcolm Wallace, Ross Patterson, Ian Lynagh,
--                          Duncan Coutts, Thomas Schilling,
--                          Johan Tibell, Mikhail Glushenkov
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:

--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.

--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.

--     * Neither the name of Isaac Jones nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

guessToolFromGhcPath :: FilePath -- ^ Tool name
                     -> FilePath -- ^ GHC exe path
                     -> IO (Maybe FilePath)
guessToolFromGhcPath toolname ghcPath
  = do let
           path              = ghcPath
           dir               = takeDirectory path
           versionSuffix     = takeVersionSuffix (dropExeExtension path)
           guessNormal       = dir </> toolname <.> exeExtension'
           guessGhcVersioned = dir </> (toolname ++ "-ghc" ++ versionSuffix)
                               <.> exeExtension'
           guessVersioned    = dir </> (toolname ++ versionSuffix)
                               <.> exeExtension'
           guesses | null versionSuffix = [guessNormal]
                   | otherwise          = [guessGhcVersioned,
                                           guessVersioned,
                                           guessNormal]
       exists <- mapM doesFileExist guesses
       return $ listToMaybe [ file | (file, True) <- zip guesses exists ]

  where takeVersionSuffix :: FilePath -> String
        takeVersionSuffix = takeWhileEndLE isSuffixChar

        isSuffixChar :: Char -> Bool
        isSuffixChar c = isDigit c || c == '.' || c == '-'

        dropExeExtension :: FilePath -> FilePath
        dropExeExtension filepath =
          case splitExtension filepath of
            (filepath', extension) | extension == exeExtension' -> filepath'
                                   | otherwise                 -> filepath

-- | @takeWhileEndLE p@ is equivalent to @reverse . takeWhile p . reverse@, but
-- is usually faster (as well as being easier to read).
takeWhileEndLE :: (a -> Bool) -> [a] -> [a]
takeWhileEndLE p = fst . foldr go ([], False)
  where
    go x (rest, done)
      | not done && p x = (x:rest, False)
      | otherwise = (rest, True)

exeExtension' :: FilePath
exeExtension' = Distribution.Simple.BuildPaths.exeExtension
