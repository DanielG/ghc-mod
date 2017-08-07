module GhcMod.Doc where

import GHC
import GhcMod.Gap (withStyle, showDocWith)
import Outputable
import Pretty (Mode(..))

showPage :: DynFlags -> PprStyle -> SDoc -> String
showPage dflag style = showDocWith dflag PageMode . withStyle dflag style

showOneLine :: DynFlags -> PprStyle -> SDoc -> String
showOneLine dflag style = showDocWith dflag OneLineMode . withStyle dflag style

getStyle :: GhcMonad m => m PprStyle
getStyle = do
    unqual <- getPrintUnqual
    return $ mkUserStyle unqual AllTheWay

styleUnqualified :: PprStyle
styleUnqualified = mkUserStyle neverQualify AllTheWay
