-- ghc-mod: Happy Haskell Hacking
-- Copyright (C) 2015  Daniel Gröber <dxld ÄT darkboxed DOT org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE CPP, RankNTypes #-}
module GhcMod.DebugLogger where

-- (c) The University of Glasgow 2005
--
-- The Glasgow Haskell Compiler License
--
-- Copyright 2002, The University Court of the University of Glasgow.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- - Redistributions of source code must retain the above copyright notice,
-- this list of conditions and the following disclaimer.
--
-- - Redistributions in binary form must reproduce the above copyright notice,
-- this list of conditions and the following disclaimer in the documentation
-- and/or other materials provided with the distribution.
--
-- - Neither name of the University nor the names of its contributors may be
-- used to endorse or promote products derived from this software without
-- specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY COURT OF THE UNIVERSITY OF
-- GLASGOW AND THE CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
-- INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
-- FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
-- UNIVERSITY COURT OF THE UNIVERSITY OF GLASGOW OR THE CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
-- DAMAGE.

import GHC
import FastString
import Pretty
import Outputable (SDoc, PprStyle, runSDoc, initSDocContext, blankLine)
import qualified Outputable
import ErrUtils

import GhcMod.Error
import GhcMod.Gap
import Prelude

debugLogAction :: (String -> IO ()) -> GmLogAction
debugLogAction putErr _reason dflags severity srcSpan style' msg
    = case severity of
      SevOutput      -> printSDoc putErr msg style'

#if __GLASGOW_HASKELL__ >= 706
      SevDump        -> printSDoc putErr (msg Outputable.$$ blankLine) style'
#endif

#if __GLASGOW_HASKELL__ >= 708
      SevInteractive -> let
          putStrSDoc = debugLogActionHPutStrDoc dflags putErr
       in
          putStrSDoc msg style'
#endif
      SevInfo        -> printErrs putErr msg style'
      SevFatal       -> printErrs putErr msg style'
      _              -> do putErr "\n"
#if __GLASGOW_HASKELL__ >= 706
                           printErrs putErr (mkLocMessage severity srcSpan msg) style'
#else
                           printErrs putErr (mkLocMessage srcSpan msg) style'
#endif
                           -- careful (#2302): printErrs prints in UTF-8,
                           -- whereas converting to string first and using
                           -- hPutStr would just emit the low 8 bits of
                           -- each unicode char.
    where
#if __GLASGOW_HASKELL__ >= 706
      printSDoc put = debugLogActionHPrintDoc  dflags put
      printErrs put = debugLogActionHPrintDoc  dflags put
#endif


#if __GLASGOW_HASKELL__ >= 706

debugLogActionHPrintDoc :: DynFlags -> (String -> IO ()) -> SDoc -> PprStyle -> IO ()
debugLogActionHPrintDoc dflags put d sty
 = debugLogActionHPutStrDoc dflags put (d Outputable.$$ Outputable.text "") sty
      -- Adds a newline

debugLogActionHPutStrDoc :: DynFlags -> (String -> IO ()) -> SDoc -> PprStyle -> IO ()
debugLogActionHPutStrDoc dflags put d sty
  = gmPrintDoc_ Pretty.PageMode (pprCols dflags) put doc
  where   -- Don't add a newline at the end, so that successive
          -- calls to this log-action can output all on the same line
    doc = runSDoc d (initSDocContext dflags sty)

#else

printSDoc = printErrs

printErrs :: (String -> IO ()) -> SDoc -> PprStyle -> IO ()
printErrs put doc sty = do
  gmPrintDoc PageMode 100 put (runSDoc doc (initSDocContext sty))

#endif

gmPrintDoc :: Mode -> Int -> (String -> IO ()) -> Doc -> IO ()
-- printDoc adds a newline to the end
gmPrintDoc mode cols put doc = gmPrintDoc_ mode cols put (doc $$ text "")

gmPrintDoc_ :: Mode -> Int -> (String -> IO ()) -> Doc -> IO ()
gmPrintDoc_ mode pprCols putS doc
  = fullRender mode pprCols 1.5 put done doc
  where
    put (Chr c)  next = putS [c] >> next
    put (Str s)  next = putS s >> next
    put (PStr s) next = putS (unpackFS s) >> next
#if __GLASGOW_HASKELL__ >= 708
    put (ZStr s) next = putS (zString s) >> next
#endif
    put (LStr s _l) next = putS (unpackLitString s) >> next

    done = return () -- hPutChar hdl '\n'
