module Container.Sum where

import Container ( IsContainer(..), Container(..) )
import Data.Functor.Sum ( Sum(..) )

type SumShape a b = Either a b

data SSumShape (sa :: a -> *) (sb :: b -> *) (s :: SumShape a b)
  where
  SL :: sa v -> SSumShape sa sb ('Left v)
  SR :: sb v -> SSumShape sa sb ('Right v)

data SumIndex (ai :: a -> *) (bi :: b -> *) (s :: SumShape a b)
  where
  LI :: ai v -> SumIndex ai bi ('Left v)
  RI :: bi v -> SumIndex ai bi ('Right v)

instance
  ( IsContainer sl ssl pl fl
  , IsContainer sr ssr pr fr
  ) =>
  IsContainer (SumShape sl sr) (SSumShape ssl ssr) (SumIndex pl pr) (Sum fl fr)
  where
  unconvert (Container s f) = case s of
    SL s -> InL $ unconvert $ Container s $ f . LI
    SR s -> InR $ unconvert $ Container s $ f . RI
  convert (InL fa) = case convert fa of
    Container s f -> Container (SL s) (f . (\(LI v) -> v))
  convert (InR fa) = case convert fa of
    Container s f -> Container (SR s) (f . (\(RI v) -> v))
