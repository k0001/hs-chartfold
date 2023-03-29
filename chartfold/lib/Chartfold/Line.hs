{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Chartfold.Line {--}
 ( -- * Line
   Line(config, points)
 , initial
 , update
   -- * Update
 , Update
 , set
 , del
   -- * Hide
 , Config(..)
 , configDefault
 , Style(..)
 , styleDefault
 , StyleCap(..)
 , styleCapDefault
 , StyleJoin(..)
 , styleJoinDefault
 ) --}
 where

import Data.AdditiveGroup
import Data.AffineSpace
import Data.Colour qualified as Co
import Data.Colour.Names qualified as Co
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Chartfold.Orphans ()

--------------------------------------------------------------------------------

data Style = Style
  { color  :: Co.AlphaColour Double
  , width  :: Double
  , dashes :: [Double]
  , cap    :: StyleCap
  , join   :: StyleJoin
  } deriving (Eq, Ord, Show)

styleDefault :: Style
styleDefault = Style
  { color  = Co.opaque Co.purple
  , width  = 1
  , dashes = []
  , cap    = styleCapDefault
  , join   = styleJoinDefault
  }

data StyleCap
  = StyleCapButt   -- ^ Just cut the line straight.
  | StyleCapRound  -- ^ Make a rounded line end.
  | StyleCapSquare -- ^ Make a square that ends the line.
  deriving (Eq, Ord, Show)

styleCapDefault :: StyleCap
styleCapDefault = StyleCapButt

data StyleJoin
  = StyleJoinMiter -- ^ Extends the outlines until they meet each other.
  | StyleJoinRound -- ^ Draw a circle fragment to connect line end.
  | StyleJoinBevel -- ^ Like 'StyleJoinMiter', but cuts off after a certain
                   --   treshold is exceeded.
  deriving (Eq, Ord, Show)

styleJoinDefault :: StyleJoin
styleJoinDefault = StyleJoinBevel

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

data Line x y = Line
  { config :: Config x
  , points :: Map x (Style, y)
  }

deriving stock instance (Eq (Diff x), Eq x, Eq y) => Eq (Line x y)
deriving stock instance (Ord (Diff x), Ord x, Ord y) => Ord (Line x y)
deriving stock instance (Show (Diff x), Show x, Show y) => Show (Line x y)

initial :: forall x y. Config x -> Line x y
initial c = Line c Map.empty

update :: forall x y. Ord x => Update x y -> Line x y -> Line x y
update (Update m) (Line c p) = Line c (Map.mapMaybe id (m <> Map.map Just p))
{-# INLINE update #-}

--------------------------------------------------------------------------------

newtype Update x y = Update (Map x (Maybe (Style, y)))

-- | @old '<>' new@ is biased towards @new@.
instance Ord x => Semigroup (Update x y) where
  Update l <> Update r = Update (r <> l)

instance Ord x => Monoid (Update x y) where
  mempty = Update mempty

deriving stock instance (Eq x, Eq y) => Eq (Update x y)
deriving stock instance (Ord x, Ord y) => Ord (Update x y)
deriving stock instance (Show x, Show y) => Show (Update x y)

-- | @'set' s x y@ sets to @y@ the line point at @x@, using style @s@.
set :: forall x y. Style -> x -> y -> Update x y
set s x y = Update (Map.singleton x (Just (s, y)))

-- | @'del' x@ deletes the line point at @x@.
del :: forall x y. x -> Update x y
del x = Update (Map.singleton x Nothing)

