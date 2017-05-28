-- ghc-mod: Happy Haskell Hacking
-- Copyright (C) 2015  Nikolay Yakimov <root@livid.pp.ru>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE OverloadedStrings, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module GhcMod.Options.Help where

import Options.Applicative
import Options.Applicative.Help.Pretty (Doc)
import qualified Options.Applicative.Help.Pretty as PP
import Control.Monad.State
import GHC.Exts( IsString(..) )
import Data.Maybe
import Data.Monoid
import Prelude

newtype MyDocM s a = MyDoc {unwrapState :: State s a}
  deriving (Monad, Functor, Applicative, MonadState s)
type MyDoc = MyDocM (Maybe Doc) ()

instance IsString (MyDocM (Maybe Doc) a) where
    fromString = append . para

instance Monoid (MyDocM (Maybe Doc) ()) where
  mappend a b = append $ doc a <> doc b
  mempty = append PP.empty

para :: String -> Doc
para = PP.fillSep . map PP.text . words

append :: Doc -> MyDocM (Maybe Doc) a
append s = modify m >> return undefined
  where
    m :: Maybe Doc -> Maybe Doc
    m Nothing = Just s
    m (Just old) = Just $ old PP..$. s

infixr 7 \\
(\\) :: MyDoc -> MyDoc -> MyDoc
(\\) a b = append $ doc a PP.<+> doc b

doc :: MyDoc -> Doc
doc = fromMaybe PP.empty . flip execState Nothing . unwrapState

help' :: MyDoc -> Mod f a
help' = helpDoc . Just . doc

desc :: MyDoc -> InfoMod a
desc = footerDoc . Just . doc . indent 2

code :: MyDoc -> MyDoc
code x = do
  _ <- " "
  indent 4 x
  " "

progDesc' :: MyDoc -> InfoMod a
progDesc' = progDescDoc . Just . doc

indent :: Int -> MyDoc -> MyDoc
indent n = append . PP.indent n . doc

int' :: Int -> MyDoc
int' = append . PP.int

para' :: String -> MyDoc
para' = append . para
