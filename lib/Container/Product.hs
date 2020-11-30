module Container.Product where

import Container ( IsContainer(..), Container(..) )
import Data.Functor.Product ( Product(..) )
import Dependent ( DSum'(..), DProduct(..) )

instance
  ( IsContainer sl ssl pl fl
  , IsContainer sr ssr pr fr
  ) =>
  IsContainer (sl, sr) (DProduct ssl ssr) (DSum' pl pr) (Product fl fr)
  where
  unconvert (Container s f) = case s of
    DPair sa sb -> Pair (unconvert $ Container sa $ f . DL') (unconvert $ Container sb $ f . DR')
  convert (Pair fa ga) = case (convert fa, convert ga) of
    (Container sl fl, Container sr fr) -> Container (DPair sl sr) $ \case
      DL' li -> fl li
      DR' ri -> fr ri
