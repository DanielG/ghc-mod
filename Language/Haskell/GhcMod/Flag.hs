module Language.Haskell.GhcMod.Flag where

import qualified Language.Haskell.GhcMod.Gap as Gap
import Language.Haskell.GhcMod.Convert
import Language.Haskell.GhcMod.Monad

-- | Listing GHC flags. (e.g -fno-warn-orphans)

flags :: IOish m => GhcModT m String
flags = convert' [ "-f" ++ prefix ++ option
                 | option <- Gap.fOptions
                 , prefix <- ["","no-"]
                 ]
