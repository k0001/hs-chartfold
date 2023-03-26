module Chartfold.Id {--}
 ( Id(..)
 ) --}
 where

import Data.Kind
import Chartfold.Orphans ()

--------------------------------------------------------------------------------

-- | Identifier for a particular 'Element'.
newtype Id (a :: Type) = Id { un :: Int }
  deriving newtype (Eq, Ord, Show)
