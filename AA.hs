{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AA where

import Control.Applicative
import Control.Exception
import Control.Monad
import CoreMonad
import Data.Typeable
import Exception
import GHC

----------------------------------------------------------------

instance Applicative Ghc where
    pure  = return
    (<*>) = ap

instance Alternative Ghc where
  empty = goNext
  x <|> y = x `gcatch` (\(_ :: SomeException) -> y)

----------------------------------------------------------------

{-| Go to the next 'Ghc' monad by throwing 'AltGhcgoNext'.
-}
goNext :: Ghc a
goNext = liftIO $ throwIO AltGhcgoNext

{-| Run any one 'Ghc' monad.
-}
runAnyOne :: [Ghc a] -> Ghc a
runAnyOne = foldr (<|>) goNext

----------------------------------------------------------------

{-| Exception to control 'Alternative' 'Ghc'.
-}
data AltGhcgoNext = AltGhcgoNext deriving (Show, Typeable)

instance Exception AltGhcgoNext
