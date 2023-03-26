{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Chartfold.Line
 ( Line(..)
 , Config(Config, title)
 , Update(Update, style, y)
 , Err
 , Style(..)
 , style
 , StyleCap(..)
 , styleCap
 , StyleJoin(..)
 , styleJoin
 ) where

import Data.AffineSpace (AffineSpace(..))
import qualified Data.Colour as Co
import qualified Data.Colour.Names as Co
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Chartfold.Core

--

data Style = Style
  { color  :: Co.AlphaColour Double
  , width  :: Double
  , dashes :: [Double]
  , cap    :: StyleCap
  , join   :: StyleJoin
  } deriving (Eq, Ord, Show)

-- | Default 'Style'.
style :: Style
style = Style
  { color  = Co.opaque Co.purple
  , width  = 1
  , dashes = []
  , cap    = styleCap
  , join   = styleJoin
  }

data StyleCap
  = StyleCapButt   -- ^ Just cut the line straight.
  | StyleCapRound  -- ^ Make a rounded line end.
  | StyleCapSquare -- ^ Make a square that ends the line.
  deriving (Eq, Ord, Show)

-- | Default 'StyleCap'.
styleCap :: StyleCap
styleCap = StyleCapButt

data StyleJoin
  = StyleJoinMiter -- ^ Extends the outlines until they meet each other.
  | StyleJoinRound -- ^ Draw a circle fragment to connect line end.
  | StyleJoinBevel -- ^ Like 'StyleJoinMiter', but cuts off after a certain
                   --   treshold is exceeded.
  deriving (Eq, Ord, Show)

-- | Default 'StyleJoin'.
styleJoin :: StyleJoin
styleJoin = StyleJoinBevel

--

data Line x y = Line
  { config :: Config (Line x y)
  , info :: Map x (Style, y)
    -- ^ We only keep the most recently set `y`.
  }

deriving stock instance (Eq (Diff x), Eq x,  Eq y) => Eq (Line x y)
deriving stock instance (Ord (Diff x), Ord x,  Ord y) => Ord (Line x y)
deriving stock instance (Show (Diff x), Show x, Show y) => Show (Line x y)

instance (AffineSpace x, Ord x) => Element x (Line x y) where
  data instance Update (Line x y) = Update
    { style :: Style
    , y     :: y
    } deriving stock (Eq, Ord, Show)

  data instance Config (Line x y) = Config
    { title :: T.Text
    , x     :: Diff x
    }

  data instance Err (Line x y)
    deriving stock (Eq, Ord, Show)
    deriving anyclass (Exception)

  element d = Line { config = d , info = mempty }

  update x us s = case us of
    Seq.Empty -> Right s
    _ Seq.:|> u -> Right $! s
       { info = Map.insert (x .+^ s.config.x) (u.style, u.y) s.info }

deriving stock instance Eq (Diff x) => Eq (Config (Line x y))
deriving stock instance Ord (Diff x) => Ord (Config (Line x y))
deriving stock instance Show (Diff x) => Show (Config (Line x y))

-- | Right-biased.
instance Semigroup (Update (Line x y)) where
  _ <> r = r
  {-# INLINE (<>) #-}
