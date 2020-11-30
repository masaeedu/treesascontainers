module Container.Sum where

import Container ( IsContainer(..), Container(..) )
import Data.Functor.Sum ( Sum(..) )
import Dependent ( DSum(..) )

instance
  ( IsContainer sl ssl pl fl
  , IsContainer sr ssr pr fr
  ) =>
  IsContainer (Either sl sr) (DSum ssl ssr) (DSum pl pr) (Sum fl fr)
  where
  unconvert (Container s f) = case s of
    DL l -> InL $ unconvert $ Container l $ f . DL
    DR r -> InR $ unconvert $ Container r $ f . DR
  convert (InL fa) = case convert fa of
    Container s f -> Container (DL s) (f . (\(DL v) -> v))
  convert (InR fa) = case convert fa of
    Container s f -> Container (DR s) (f . (\(DR v) -> v))
