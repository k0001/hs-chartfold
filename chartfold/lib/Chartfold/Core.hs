module Chartfold.Core
 ( Id(..)
 , Element(..)
 , ConfigAdd(..)
 ) where

import Data.Kind
import Data.Sequence (Seq)

import Chartfold.Orphans ()

--------------------------------------------------------------------------------

-- | An 'Element' can be updated on every frame (@x@).
class Element (x :: Type) (a :: Type) | a -> x where
  -- | 'Config'uration for the 'Element'. Stays constant over time.
  data Config a :: Type
  -- | 'update' 'Err'ors.
  data Err a :: Type
  -- | Description of a single frame 'Update'.
  data Update a :: Type
  -- | Initial 'Element'.
  element :: Config a -> a
  -- | Apply 'Update's to an 'Element' for a particular frame @x@.
  --
  -- The rightmost 'Update', which is the most recent, has precedence over
  -- previous one.
  update :: x -> Seq (Update a) -> a -> Either (Err a) a

--

-- | Identifier for a particular 'Element'.
newtype Id (a :: Type) = Id { un :: Int }
  deriving stock (Eq, Ord, Show)

--

class ConfigAdd x a b where
  configAdd :: Config b
            -> Config a
            -> ( x -> Seq (Update b) -> a -> Either (Err a) a
               , Config a
               ) -- ^

