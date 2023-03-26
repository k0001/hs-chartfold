{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Chartfold.Constraint
 ( Entails(..)
 , Entails1
 ) where

import Data.Constraint

-- Unrelated import just so that downstream gets orphans from this module too.
import Chartfold.Orphans ()

--------------------------------------------------------------------------------

type Entails1 (a :: k -> Constraint) (b :: k -> Constraint) =
  forall (x :: k). Entails (a x) (b x)

-- | @a ':-' b@, as a 'Constraint'.
class Entails (a :: Constraint) (b :: Constraint) where
  entails :: a :- b
  -- | Default instance for cases where @b@ is readily satisfied.
  default entails :: b => a :- b
  entails = Sub Dict
  {-# INLINE entails #-}

-- Does this work?
instance (Typeable x) => Entails a (Typeable x)
instance (Show x) => Entails a (Show x)
instance (Eq x) => Entails a (Eq x)
instance (Ord x) => Entails a (Ord x)

