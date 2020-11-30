module Dependent where

data DSum (f :: a -> *) (g :: b -> *) (x :: Either a b)
  where
  DL :: f a -> DSum f g ('Left a)
  DR :: g a -> DSum f g ('Right a)

data DSum' (f :: a -> *) (g :: b -> *) (x :: (a, b))
  where
  DL' :: f a -> DSum' f g '(a, b)
  DR' :: g b -> DSum' f g '(a, b)

data DProduct (f :: a -> *) (g :: b -> *) (x :: (a, b))
  where
  DPair :: f a -> g b -> DProduct f g '(a, b)
