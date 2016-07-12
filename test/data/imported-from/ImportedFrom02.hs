-- ImportedFrom02.hs

module ImportedFrom02 where

import Data.List hiding (map)
import System.Environment (getArgs)
import qualified Safe






m = map (+1) [1, 2, 3]

h = head [1, 2, 3]

h' = Safe.headMay []

