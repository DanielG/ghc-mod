module Language.Haskell.GhcMod.Doc where

import GHC
import Language.Haskell.GhcMod.Gap (withStyle, showDocWith)
import Outputable
import Pretty (Mode(..))

showPage :: DynFlags -> PprStyle -> SDoc -> String
showPage dflag style = showDocWith dflag PageMode . withStyle dflag style

showOneLine :: DynFlags -> PprStyle -> SDoc -> String
showOneLine dflag style = showDocWith dflag OneLineMode . withStyle dflag style

-- showForUser :: DynFlags -> PrintUnqualified -> SDoc -> String
-- showForUser dflags unqual sdoc =
--     showDocWith dflags PageMode $
--       runSDoc sdoc $ initSDocContext dflags $ mkUserStyle unqual AllTheWay

getStyle :: GhcMonad m => m PprStyle
getStyle = do
    unqual <- getPrintUnqual
    return $ mkUserStyle unqual AllTheWay

styleUnqualified :: PprStyle
styleUnqualified = mkUserStyle neverQualify AllTheWay
