module Container.Product where

import Container ( IsContainer(..), Container(..) )
import Data.Functor.Product ( Product(..) )

data SProductShape (sa :: a -> *) (sb :: b -> *) (s :: (a, b))
  where
  SProductShape :: sa a -> sb b -> SProductShape sa sb '(a, b)

data ProductIndex (ai :: a -> *) (bi :: b -> *) (s :: (a, b))
  where
  LPI :: ai a -> ProductIndex ai bi '(a, b)
  RPI :: bi b -> ProductIndex ai bi '(a, b)

instance
  ( IsContainer sl ssl pl fl
  , IsContainer sr ssr pr fr
  ) =>
  IsContainer (sl, sr) (SProductShape ssl ssr) (ProductIndex pl pr) (Product fl fr)
  where
  unconvert (Container s f) = case s of
    SProductShape sa sb -> Pair (unconvert $ Container sa $ f . LPI) (unconvert $ Container sb $ f . RPI)
  convert (Pair fa ga) = case (convert fa, convert ga) of
    (Container sl fl, Container sr fr) -> Container (SProductShape sl sr) $ \case
      LPI li -> fl li
      RPI ri -> fr ri
