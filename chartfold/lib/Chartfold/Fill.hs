{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Chartfold.Fill {--}
 ( Fill(..)
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
  } deriving stock (Eq, Ord, Show)

styleDefault :: Style
styleDefault = Style
  { color = Co.withOpacity Co.blue 0.6
  }

data Fill x y = Fill
  { config :: Config
  , info   :: Map Style (Map x (Set (y, y)))
  } deriving stock (Eq, Ord, Show)

data Update x y = Update
  { style :: Style
  , x     :: Diff x
  , y     :: (y, y)
  }

deriving stock instance (Eq (Diff x), Eq y) => Eq (Update x y)
deriving stock instance (Ord (Diff x), Ord y) => Ord (Update x y)
deriving stock instance (Show (Diff x), Show y) => Show (Update x y)

newtype Config = Config
    { title :: T.Text
    } deriving newtype (Eq, Ord)
      deriving stock (Show)

initial :: forall x y. Config -> Fill x y
initial d = Fill { config = d , info = mempty }

update
  :: forall x y
  .  (AffineSpace x, Ord x, Ord y)
  => x
  -> Seq (Update x y) -- ^ Rightmost is recentmost.
  -> Fill x y
  -> Fill x y
update x us s = s { info = foldl' f s.info us }
  where f m u = let !x' = x .+^ u.x
                    !ys = Set.singleton (uncurry min u.y, uncurry max u.y)
                in Map.alter (Just . maybe (Map.singleton x' ys)
                                           (Map.insertWith mappend x' ys))
                             u.style
                             m

