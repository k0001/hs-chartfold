{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Chartfold.Fill {--}
 ( -- * Fill
   Fill
 , initial
 , update
   -- * Update
 , Update
 , Chartfold.Fill.show
   -- * Config
 , Config(..)
 , configDefault
 , Style(..)
 , styleDefault
 ) --}
 where

import Data.AdditiveGroup (AdditiveGroup(zeroV))
import Data.AffineSpace (AffineSpace(Diff))
import Data.Colour qualified as Co
import Data.Colour.Names qualified as Co
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Text qualified as T

import Chartfold.Extra (Interval)

--------------------------------------------------------------------------------

data Style = Style
  { color :: Co.AlphaColour Double
  } deriving stock (Eq, Ord, Show)

styleDefault :: Style
styleDefault = Style
  { color = Co.withOpacity Co.blue 0.6
  }

--------------------------------------------------------------------------------

data Config x = Config
  { title :: T.Text
  , xoff  :: Diff x
  }

deriving stock instance (Eq (Diff x)) => Eq (Config x)
deriving stock instance (Ord (Diff x)) => Ord (Config x)
deriving stock instance (Show (Diff x)) => Show (Config x)

configDefault :: AdditiveGroup (Diff x) => T.Text -> Config x
configDefault title = Config{title, xoff=zeroV}

--------------------------------------------------------------------------------

newtype Update x y = Update (Map x (Style, Interval y))

-- | @old '<>' new@
instance Ord x => Semigroup (Update x y) where
  Update l <> Update r = Update (r <> l)

instance Ord x => Monoid (Update x y) where
  mempty = Update mempty

deriving stock instance (Eq x, Eq y) => Eq (Update x y)
deriving stock instance (Ord x, Ord y) => Ord (Update x y)
deriving stock instance (Show x, Show y) => Show (Update x y)

-- | @'show' s x y0 y1@ fills the space between @y0@ and @y1@
-- at @x@ with style @s@.
show :: forall x y. Style -> x -> Interval y -> Update x y
show s x y = Update (Map.singleton x (s, y))
{-# INLINE show #-}

{- TODO
-- | @'hide' x y0 y1@ hides the fill any previously 'show'n space
-- betwen the @y0@ and @y1@ at @x@.
hide :: forall x y. x -> y -> y -> Update x y
hide x y0 y1 = Update (Map.singleton x (min y0 y1, max y0 y1, Nothing))
{-# INLINE hide #-}
-}


--------------------------------------------------------------------------------

data Fill x y = Fill
  { _config :: Config x
  , _points :: Map Style (Map x (Seq (Interval y)))
  }

instance HasField "config" (Fill x y) (Config x) where
  getField = (._config)
instance HasField "points" (Fill x y) (Map Style (Map x (Seq (Interval y)))) where
  getField = (._points)

deriving stock instance (Eq x, Eq (Diff x), Eq y) => Eq (Fill x y)
deriving stock instance (Ord x, Ord (Diff x), Ord y) => Ord (Fill x y)
deriving stock instance (Show x, Show (Diff x), Show y) => Show (Fill x y)

initial :: forall x y. Config x -> Fill x y
initial c = Fill c Map.empty

update :: forall x y. Ord x => Update x y -> Fill x y -> Fill x y
update = \(Update m) (Fill c p) -> Fill c (Map.foldrWithKey f p m)
  where f :: x
          -> (Style, Interval y)
          -> Map Style (Map x (Seq (Interval y)))
          -> Map Style (Map x (Seq (Interval y)))
        f x (s, y) = Map.insertWith (Map.unionWith mappend) s $
                       Map.singleton x (pure y)

