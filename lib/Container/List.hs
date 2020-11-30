{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Container.List where

import Container ( IsContainer(..), Container(..) )

-- We already have the List type in base:
-- data [a] = [] | a : [a]

-- The shape of a list is given by its length
data Nat = Z | S Nat

-- Here is the corresponding singleton
data SNat (n :: Nat)
  where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

-- The positions of a list correspond to natural numbers strictly smaller than its length
data Fin (n :: Nat)
  where
  FZ :: Fin ('S n)          -- Zero is smaller than any non-zero natural number
  FS :: Fin n -> Fin ('S n) -- Any number is smaller than its successor

type List' = Container Nat SNat Fin

-- Constructors
nil :: List' a
nil = Container SZ $ \case {} -- This is total because there is no natural number smaller than zero

cons :: a -> List' a -> List' a
cons x (Container l f) = Container (SS l) $ \case
  FZ -> x
  FS i -> f i

-- Deconstructor
matchList :: x -> (a -> List' a -> x) -> List' a -> x
matchList nil' cons' (Container l f) = case l of
  SZ -> nil'
  SS l' -> cons' (f FZ) $ Container l' (f . FS)

instance IsContainer Nat SNat Fin []
  where
  unconvert = matchList [] (\x xs -> x : unconvert xs)
  convert [] = nil
  convert (x : xs) = cons x (convert xs)
