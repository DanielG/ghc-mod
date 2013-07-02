module Language.Haskell.GhcMod.Doc where

import DynFlags (DynFlags)
import Language.Haskell.GhcMod.Gap (withStyle, styleUnqualified)
import Outputable
import Pretty

styleQualified :: PprStyle
styleQualified = mkUserStyle alwaysQualify AllTheWay

showQualifiedPage :: DynFlags -> SDoc -> String
showQualifiedPage dflag = showDocWith PageMode . withStyle dflag styleQualified

showQualifiedOneLine :: DynFlags -> SDoc -> String
showQualifiedOneLine dflag = showDocWith OneLineMode . withStyle dflag styleQualified

showUnqualifiedOneLine :: DynFlags -> SDoc -> String
showUnqualifiedOneLine dflag = showDocWith OneLineMode . withStyle dflag styleUnqualified
