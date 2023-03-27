{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Chartfold.Line {--}
 ( Line(..)
 , update
 , Update(..)
 , initial
 , Config(..)
 , Style(..)
 , styleDefault
 , StyleCap(..)
 , styleCapDefault
 , StyleJoin(..)
 , styleJoinDefault
 ) --}
 where

import Data.AffineSpace (AffineSpace(..))
import Data.Colour qualified as Co
import Data.Colour.Names qualified as Co
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq(..))
import Data.Text qualified as T

import Chartfold.Orphans ()

--

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

--

data Line x y = Line
  { config :: Config x
  , info :: Map x (Style, y)
    -- ^ We only keep the most recently set `y`.
  }

deriving stock instance (Eq (Diff x), Eq x,  Eq y) => Eq (Line x y)
deriving stock instance (Ord (Diff x), Ord x,  Ord y) => Ord (Line x y)
deriving stock instance (Show (Diff x), Show x, Show y) => Show (Line x y)

data Update y = Update
  { style :: Style
  , y     :: y
  } deriving stock (Eq, Ord, Show)

-- | @old '<>' new@
instance Semigroup (Update y) where
  _ <> r = r
  {-# INLINE (<>) #-}

data Config x = Config
  { title :: T.Text
  , x     :: Diff x -- ^ This is an offset so that the line is not rendered at @x@,
                    -- but at @x - 'Config'.x@. Do we need this? Hopefully not, and we
                    -- can remove it.
  }

deriving stock instance Eq (Diff x) => Eq (Config x)
deriving stock instance Ord (Diff x) => Ord (Config x)
deriving stock instance Show (Diff x) => Show (Config x)

initial :: forall x y. Ord x => Config x -> Line x y
initial d = Line { config = d, info = mempty }

update
  :: forall x y
  .  (AffineSpace x, Ord x)
  => x
  -> Seq (Update y)
  -> Line x y
  -> Line x y
update x us s = case us of
  _ :|> u -> s { info = Map.insert (x .+^ s.config.x) (u.style, u.y) s.info }
  _       -> s

