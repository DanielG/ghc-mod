module Doc where

import DynFlags (DynFlags)
import Gap (withStyle)
import Outputable
import Pretty

styleQualified :: PprStyle
styleQualified = mkUserStyle alwaysQualify AllTheWay

styleUnqualified :: PprStyle
styleUnqualified = mkUserStyle neverQualify AllTheWay

showQualifiedPage :: DynFlags -> SDoc -> String
showQualifiedPage dflag = showDocWith PageMode . withStyle dflag styleQualified

showUnqualifiedPage :: DynFlags -> SDoc -> String
showUnqualifiedPage dflag = showDocWith PageMode . withStyle dflag styleUnqualified

showQualifiedOneLine :: DynFlags -> SDoc -> String
showQualifiedOneLine dflag = showDocWith OneLineMode . withStyle dflag styleQualified

showUnqualifiedOneLine :: DynFlags -> SDoc -> String
showUnqualifiedOneLine dflag = showDocWith OneLineMode . withStyle dflag styleUnqualified
