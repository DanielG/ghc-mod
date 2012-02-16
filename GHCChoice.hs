{-# LANGUAGE ScopedTypeVariables #-}

module GHCChoice where

import Control.Exception
import CoreMonad
import Exception
import GHC

----------------------------------------------------------------

(||>) :: Ghc a -> Ghc a -> Ghc a
x ||> y = x `gcatch` (\(_ :: IOException) -> y)

----------------------------------------------------------------

{-| Go to the next 'Ghc' monad by throwing 'AltGhcgoNext'.
-}
goNext :: Ghc a
goNext = liftIO . throwIO $ userError "goNext"

{-| Run any one 'Ghc' monad.
-}
runAnyOne :: [Ghc a] -> Ghc a
runAnyOne = foldr (||>) goNext
