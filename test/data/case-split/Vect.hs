{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, GADTs, KindSignatures #-}

module Vect where

data Nat = Z | S Nat

type family   (n :: Nat) :+ (m :: Nat) :: Nat
type instance Z :+ m = m
type instance S n :+ m = S (n :+ m)

data Vect :: Nat -> * -> * where
  VNil :: Vect Z a
  (:::) :: a -> Vect n a -> Vect (S n) a

vAppend :: Vect n a -> Vect m a -> Vect (n :+ m) a
vAppend x y = _vAppend_body

lAppend :: [a] -> [a] -> [a]
lAppend x y = _lAppend_body

data MyList a = Nil | Cons a (MyList a)

mlAppend :: MyList a -> MyList a -> MyList a
mlAppend x y = _mlAppend_body
