{-# LANGUAGE LambdaCase, PartialTypeSignatures #-}

module Lib where

import Data.Monoid

-- Here is how we define a standard binary tree
data OGTree a = OGLeaf a | OGBranch (OGTree a) (OGTree a)

-- Now we try to shove trees into the framework outlined by "categories of containers"

-- A container functor is a dependent sum of a shape and a function from a shape-appropriate
-- position to contained values.
data Container (s :: *) (proxy :: s -> *) (p :: s -> *) (a :: *) = forall (x :: s). Container (proxy x) (p x -> a)

-- Here is the shape for trees
data TreeShape = Leaf | Branch TreeShape TreeShape

-- Here is the mechanically derived singleton for the tree shape (so we can case analyze at the term level)
-- NB: This is unnecessary in the presence of proper dependent sums
data STreeShape (s :: TreeShape)
  where
  SLeaf :: STreeShape 'Leaf
  SBranch :: STreeShape l -> STreeShape r -> STreeShape ('Branch l r)

-- Here is the shape-indexed family of position types for trees
-- This should basically be a list of booleans "of an appropriate length"
data TreeIndex (shape :: TreeShape)
  where
  Root :: TreeIndex 'Leaf                        -- the "nil" case
  L    :: TreeIndex l -> TreeIndex ('Branch l r) -- "consing" a 'True
  R    :: TreeIndex r -> TreeIndex ('Branch l r) -- "consing" a 'False

-- And here at last is the tree functor
type Tree = Container TreeShape STreeShape TreeIndex

-- We can recover the familiar constructors for a tree...

-- Just a leaf node
leaf :: a -> Tree a
leaf i = Container SLeaf $ \case
  Root -> i

-- A branch containing two trees
branch :: Tree a -> Tree a -> Tree a
branch (Container sl f) (Container sr g) =
  Container (SBranch sl sr) $ \case
    L l -> f l
    R r -> g r

-- And the familiar deconstructor for a tree...

-- Pattern match on a tree
matchTree :: (a -> x) -> (Tree a -> Tree a -> x) -> Tree a -> x
matchTree leaf' branch' (Container s f) = case s of
  SLeaf -> leaf' $ f Root
  SBranch x y -> branch'
    (Container x $ f . L)
    (Container y $ f . R)

-- Here we see how to construct a sample tree...
test :: Tree Int
test = leaf 1 `branch` (leaf 2 `branch` leaf 3) `branch` leaf 4

-- And here we see how to deconstruct it...
foldMapTree :: Monoid m => (a -> m) -> Tree a -> m
foldMapTree f = matchTree f (\a b -> foldMapTree f a <> foldMapTree f b)

message :: String
message = "The sum of the numbers in the tree is: " <> show (foldMapTree Sum test)
