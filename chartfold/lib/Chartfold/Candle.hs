{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Chartfold.Candle {--}
 ( Candle
 , initial
 , update
 , Update
 , add
 , del
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
import Data.Text qualified as T

import Chartfold.Extra

--------------------------------------------------------------------------------

data Style = Style
  { fillColor  :: Co.AlphaColour Double
  , lineColor  :: Co.AlphaColour Double
  , lineWidth  :: Double
  , lineDashes :: [Double]
  } deriving (Eq, Ord, Show)

styleDefault :: Style
styleDefault = Style
  { fillColor  = Co.opaque Co.white
  , lineColor  = Co.opaque Co.black
  , lineWidth  = 1
  , lineDashes = []
  }

--------------------------------------------------------------------------------

data Candle x y = Candle
  { _config :: Config x
  , _points :: Map (Interval x) (Style, OHLC y)
  }

instance HasField "config" (Candle x y) (Config x) where
  getField = (._config)
instance HasField "points" (Candle x y) (Map (Interval x) (Style, OHLC y)) where
  getField = (._points)

deriving stock instance (Eq x, Eq (Diff x), Eq y) => Eq (Candle x y)
deriving stock instance (Ord x, Ord (Diff x), Ord y) => Ord (Candle x y)
deriving stock instance (Show x, Show (Diff x), Show y) => Show (Candle x y)

initial :: forall x y. Config x -> Candle x y
initial c = Candle c Map.empty
{-# INLINE initial #-}

update :: forall x y. (Ord x) => Update x y -> Candle x y -> Candle x y
update (Update u) (Candle c m) = Candle c (Map.mapMaybe id (u <> fmap Just m))
{-# INLINE update #-}

--------------------------------------------------------------------------------

newtype Update x y = Update (Map (Interval x) (Maybe (Style, OHLC y)))

deriving stock instance (Eq x, Eq y) => Eq (Update x y)
deriving stock instance (Ord x, Ord y) => Ord (Update x y)
deriving stock instance (Show x, Show y) => Show (Update x y)

-- | @old '<>' new@ is biased towards @new@.
instance Ord x => Semigroup (Update x y) where
  Update l <> Update r = Update (r <> l)

instance Ord x => Monoid (Update x y) where
  mempty = Update mempty

-- | @'add' s x y@ adds a @y@ candle with style @s@ at @x@.
add :: forall x y. (Ord x) => Style -> Interval x -> OHLC y -> Update x y
add s x y = Update (Map.singleton x (Just (s, y)))
{-# INLINE add #-}

-- | @'del' x@ deletes the candle exactly at @x@.
del :: forall x y. Interval x -> Update x y
del x = Update (Map.singleton x Nothing)
{-# INLINE del #-}

--------------------------------------------------------------------------------

data Config x = Config
  { title :: T.Text
  , xoff  :: Diff x
  }

deriving stock instance (Eq (Diff x)) => Eq (Config x)
deriving stock instance (Ord (Diff x)) => Ord (Config x)
deriving stock instance (Show (Diff x)) => Show (Config x)

