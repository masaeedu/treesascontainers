module Container.Identity where

import Container ( IsContainer(..), Container(..) )

import Data.Functor.Identity ( Identity(..) )

import Data.Proxy ( Proxy(..) )

-- The shape, singleton and family of positions are all trivial
type IdentityShape = ()

type SIdentityShape = Proxy

type IdentityIndex = Proxy

-- So Identity is a container functor
instance IsContainer IdentityShape SIdentityShape IdentityIndex Identity
  where
  unconvert (Container Proxy f) = Identity $ f Proxy
  convert (Identity a) = Container Proxy $ \Proxy -> a
