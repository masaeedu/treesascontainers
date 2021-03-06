module Container where

data Container
  (shape :: *)
  (singleton :: shape -> *)
  (position :: shape -> *)
  (a :: *)
  = forall (x :: shape). Container (singleton x) (position x -> a)

-- A container is (isomorphic to) a dependent sum of a shape and a function from a shape-appropriate position to values.
class IsContainer
  (shape :: *)
  (singleton :: shape -> *)
  (position :: shape -> *)
  (f :: * -> *)
  | f -> shape singleton position
  , shape singleton position -> f
  where
  convert :: f a -> Container shape singleton position a
  unconvert :: Container shape singleton position a -> f a

  -- Laws:
  -- convert . unconvert = id
  -- unconvert . convert = id
