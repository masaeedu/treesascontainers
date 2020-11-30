module Container.Identity where

import Container ( IsContainer(..), Container(..) )

import Data.Functor.Identity ( Identity(..) )

import Data.Proxy ( Proxy(..) )

-- The shape, singleton and family of positions are all trivial, so Identity is a container functor
instance IsContainer () Proxy Proxy Identity
  where
  unconvert (Container Proxy f) = Identity $ f Proxy
  convert (Identity a) = Container Proxy $ \Proxy -> a
