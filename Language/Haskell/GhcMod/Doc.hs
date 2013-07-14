module Language.Haskell.GhcMod.Doc where

import DynFlags (DynFlags)
import Language.Haskell.GhcMod.Gap (withStyle, styleUnqualified)
import Outputable
import Pretty

styleQualified :: PprStyle
styleQualified = mkUserStyle alwaysQualify AllTheWay

-- For "ghc-mod type"
showQualifiedPage :: DynFlags -> SDoc -> String
showQualifiedPage dflag = showDocWith PageMode . withStyle dflag styleQualified

-- Not used
showQualifiedOneLine :: DynFlags -> SDoc -> String
showQualifiedOneLine dflag = showDocWith OneLineMode . withStyle dflag styleQualified

-- To write Haskell code in a buffer
showUnqualifiedOneLine :: DynFlags -> SDoc -> String
showUnqualifiedOneLine dflag = showDocWith OneLineMode . withStyle dflag styleUnqualified
