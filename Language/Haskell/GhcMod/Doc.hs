module Language.Haskell.GhcMod.Doc where

import GHC (Ghc, DynFlags)
import qualified GHC as G
import Language.Haskell.GhcMod.Gap (withStyle, showDocWith)
import Outputable (SDoc, PprStyle, mkUserStyle, Depth(AllTheWay), neverQualify)
import Pretty (Mode(..))

showPage :: DynFlags -> PprStyle -> SDoc -> String
showPage dflag style = showDocWith dflag PageMode . withStyle dflag style

showOneLine :: DynFlags -> PprStyle -> SDoc -> String
showOneLine dflag style = showDocWith dflag OneLineMode . withStyle dflag style

getStyle :: Ghc PprStyle
getStyle = do
    unqual <- G.getPrintUnqual
    return $ mkUserStyle unqual AllTheWay

styleUnqualified :: PprStyle
styleUnqualified = mkUserStyle neverQualify AllTheWay
