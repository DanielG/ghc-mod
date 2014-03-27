module Language.Haskell.GhcMod.Doc where

import DynFlags (DynFlags)
import Language.Haskell.GhcMod.Gap (withStyle, showDocWith)
import Outputable (SDoc, PprStyle, Depth(AllTheWay), mkUserStyle, alwaysQualify, neverQualify)
import Pretty (Mode(..))

----------------------------------------------------------------

{-
pretty :: Outputable a => a -> String
pretty = showSDocForUser neverQualify . ppr

debug :: Outputable a => a -> b -> b
debug x v = trace (pretty x) v
-}

----------------------------------------------------------------

styleQualified :: PprStyle
styleQualified = mkUserStyle alwaysQualify AllTheWay

styleUnqualified :: PprStyle
styleUnqualified = mkUserStyle neverQualify AllTheWay

----------------------------------------------------------------

-- For "ghc-mod type"
showQualifiedPage :: DynFlags -> SDoc -> String
showQualifiedPage dflag = showDocWith dflag PageMode . withStyle dflag styleQualified

-- For "ghc-mod browse" and show GHC's error messages.
showUnqualifiedPage :: DynFlags -> SDoc -> String
showUnqualifiedPage dflag = showDocWith dflag PageMode
                          . withStyle dflag styleUnqualified

-- Not used
showQualifiedOneLine :: DynFlags -> SDoc -> String
showQualifiedOneLine dflag = showDocWith dflag OneLineMode . withStyle dflag styleQualified

-- To write Haskell code in a buffer
showUnqualifiedOneLine :: DynFlags -> SDoc -> String
showUnqualifiedOneLine dflag = showDocWith dflag OneLineMode . withStyle dflag styleUnqualified
