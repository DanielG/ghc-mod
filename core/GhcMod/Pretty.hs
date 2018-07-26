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

{-# LANGUAGE CPP #-}

module GhcMod.Pretty
  ( renderGm
  , renderSDoc
  , gmComponentNameDoc
  , gmLogLevelDoc
  , (<+>:)
  , fnDoc
  , showToDoc
  , warnDoc
  , strLnDoc
  , strDoc
  ) where

import Control.Arrow hiding ((<+>))
import Data.Char
import Data.List
import Distribution.Helper
import Pretty
import GHC
import Outputable (SDoc, withPprStyleDoc)

import GhcMod.Types
import GhcMod.Doc
import GhcMod.Gap (renderGm)
import Prelude hiding ( (<>))

renderSDoc :: GhcMonad m => SDoc -> m Doc
renderSDoc sdoc = do
  df <- getSessionDynFlags
  ppsty <- getStyle
  return $ withPprStyleDoc df ppsty sdoc

gmComponentNameDoc :: ChComponentName -> Doc
gmComponentNameDoc ChSetupHsName   = text $ "Setup.hs"
#if MIN_VERSION_cabal_helper(0,8,0)
gmComponentNameDoc ChLibName       = text $ "library"
gmComponentNameDoc (ChSubLibName _)= text $ "library"
gmComponentNameDoc (ChFLibName _)  = text $ "flibrary"
#else
gmComponentNameDoc (ChLibName "")  = text $ "library"
gmComponentNameDoc (ChLibName n)   = text $ "library:" ++ n
#endif
gmComponentNameDoc (ChExeName n)   = text $ "exe:" ++ n
gmComponentNameDoc (ChTestName n)  = text $ "test:" ++ n
gmComponentNameDoc (ChBenchName n) = text $ "bench:" ++ n

gmLogLevelDoc :: GmLogLevel -> Doc
gmLogLevelDoc GmSilent    = error "GmSilent MUST not be used for log messages"
gmLogLevelDoc GmPanic     = text "PANIC"
gmLogLevelDoc GmException = text "EXCEPTION"
gmLogLevelDoc GmError     = text "ERROR"
gmLogLevelDoc GmWarning   = text "Warning"
gmLogLevelDoc GmInfo      = text "info"
gmLogLevelDoc GmDebug     = text "DEBUG"
gmLogLevelDoc GmVomit     = text "VOMIT"

infixl 6 <+>:
(<+>:) :: Doc -> Doc -> Doc
a <+>: b = (a <> colon) <+> b

fnDoc :: FilePath -> Doc
fnDoc = doubleQuotes . text

showToDoc :: Show a => a -> Doc
showToDoc = strLnDoc . show

warnDoc :: Doc -> Doc
warnDoc d = text "Warning" <+>: d

strLnDoc :: String -> Doc
strLnDoc str = doc (dropWhileEnd isSpace str)
 where
   doc = lines >>> map text >>> foldr ($+$) empty

strDoc :: String -> Doc
strDoc str = doc (dropWhileEnd isSpace str)
 where
   doc :: String -> Doc
   doc = lines
         >>> map (words >>> map text >>> fsep)
         >>> \l -> case l of (x:xs) -> hang x 4 (vcat xs); [] -> empty
