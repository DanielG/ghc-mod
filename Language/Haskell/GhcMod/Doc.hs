module Language.Haskell.GhcMod.Doc where

import GHC
import Outputable

getStyle :: GhcMonad m => m PprStyle
getStyle = do
    unqual <- getPrintUnqual
    return $ mkUserStyle unqual AllTheWay

styleUnqualified :: PprStyle
styleUnqualified = mkUserStyle neverQualify AllTheWay
