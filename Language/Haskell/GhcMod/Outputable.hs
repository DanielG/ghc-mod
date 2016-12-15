{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Language.Haskell.GhcMod.Outputable where

import Language.Haskell.GhcMod.Types

data NoOutputConfig = NoOutputConfig

data OutputFormat = PlainFormat

class Outputable config a where
  showOutput :: IOish m => OutputFormat -> config -> a -> m String

instance Outputable NoOutputConfig String where
  showOutput PlainFormat _ = return
