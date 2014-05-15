module Language.Haskell.GhcMod.Flag where

import qualified Language.Haskell.GhcMod.Gap as Gap
import Language.Haskell.GhcMod.Convert
import Language.Haskell.GhcMod.Types

-- | Listing GHC flags. (e.g -fno-warn-orphans)

listFlags :: Options -> IO String
listFlags opt = return $ convert opt [ "-f" ++ prefix ++ option
                                     | option <- Gap.fOptions
                                     , prefix <- ["","no-"]
                                     ]
