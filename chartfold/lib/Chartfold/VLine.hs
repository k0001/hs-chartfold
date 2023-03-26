{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Chartfold.VLine {--}
 ( VLine(..)
 , update
 , Update(..)
 , initial
 , Config(..)
 , Style(..)
 , styleDefault
 ) --}
 where

import Data.AffineSpace (AffineSpace(..))
import Data.Colour qualified as Co
import Data.Colour.Names qualified as Co
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T

import Chartfold.Orphans ()

--------------------------------------------------------------------------------

data Style = Style
  { color :: Co.AlphaColour Double
  , width :: Double
  , dashes :: [Double]
  } deriving (Eq, Ord, Show)

styleDefault :: Style
styleDefault = Style
  { color = Co.opaque Co.blue
  , width = 1
  , dashes = [1, 3]
  }

data VLine x = VLine
  { config :: Config
  , info :: Map Style (Set x)
  } deriving stock (Eq, Ord, Show)

data Update x = Update
  { style :: Style
  , x :: Diff x
  }

{- TODO Is this OK?
-- | Right-biased.
instance Semigroup (Update x) where
  _ <> r = r
  {-# INLINE (<>) #-}
-}

deriving stock instance (Eq (Diff x)) => Eq (Update x)
deriving stock instance (Ord (Diff x)) => Ord (Update x)
deriving stock instance (Show (Diff x)) => Show (Update x)

newtype Config = Config
  { title :: T.Text
  } deriving stock (Eq, Ord, Show)

initial :: Config -> VLine x
initial d = VLine { config = d, info = mempty }

update
  :: forall x
  .  (AffineSpace x, Ord x)
  => x
  -> Seq (Update x) -- ^ Rightmost is recentmost.
  -> VLine x
  -> VLine x
update x us s = s { info = foldl' f s.info us }
  where f m u = Map.insertWith mappend u.style (Set.singleton $! x .+^ u.x) m


