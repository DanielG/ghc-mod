module Language.Haskell.GhcMod.Doc where

import GHC
import Language.Haskell.GhcMod.Gap (withStyle, showDocWith)
import Outputable
import Pretty (Mode(..))

showPage :: DynFlags -> PprStyle -> SDoc -> String
showPage dflag style = showDocWith dflag PageMode . withStyle dflag style

showOneLine :: DynFlags -> PprStyle -> SDoc -> String
showOneLine dflag style = showDocWith dflag OneLineMode . withStyle dflag style

getStyle :: GhcMonad m => DynFlags -> m PprStyle
getStyle df = do
    unqual <- getPrintUnqual
    return $ mkUserStyle df unqual AllTheWay

styleUnqualified :: DynFlags -> PprStyle
styleUnqualified df = mkUserStyle df neverQualify AllTheWay
