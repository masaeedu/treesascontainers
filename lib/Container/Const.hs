module Container.Const where

import Container ( IsContainer(..), Container(..) )

import Data.Functor.Const ( Const(..) )
import Data.Void ( Void, absurd )
import Data.Functor.Identity ( Identity(..) )

instance IsContainer () (Const r) (Const Void) (Const r)
  where
  unconvert (Container (Const s) f) = Const s
  convert (Const x) = Container (Const x) (absurd . getConst)
