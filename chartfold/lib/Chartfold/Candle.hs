{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Chartfold.Candle {--}
 ( Candle(..)
 , update
 , Update(..)
 , initial
 , Config(..)
 , Style(..)
 , styleDefault
 , Err(..)
 ) --}
 where

import Data.AffineSpace (AffineSpace(..))
import Data.Colour qualified as Co
import Data.Colour.Names qualified as Co
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Text qualified as T

import Chartfold.Orphans ()

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

data Candle x y = Candle
  { config :: Config
  , info   :: Map (x, x) (Style, y, y, y, y)
    -- ^ (start, end) -> (_, open, high, low, close).
    --
    -- We only keep the most recently set candle.
  } deriving stock (Eq, Ord, Show)

data Update x y = Update
  { style :: Style
  , start :: Diff x
  , end   :: Diff x
  , open  :: y
  , high  :: y
  , low   :: y
  , close :: y
  }

deriving stock instance (Eq (Diff x), Eq y) => Eq (Update x y)
deriving stock instance (Ord (Diff x), Ord y) => Ord (Update x y)
deriving stock instance (Show (Diff x), Show y) => Show (Update x y)

newtype Config = Config
  { title :: T.Text
  } deriving newtype (Eq, Ord)
    deriving stock (Show)

data Err x y
  = Err_StartEnd x x
    -- ^ If candle start time isn't before end time.
    -- /Start, end./
  | Err_OHLC y y y y
    -- ^ If low > min(open, close) or high < max(open, close).
    -- /Open, high, low, close./
  deriving stock (Eq, Ord, Show)
  deriving anyclass (Exception)

initial :: forall x y. Ord x => Config -> Candle x y
initial d = Candle { config = d, info = mempty }

update
  :: forall x y
  .  (AffineSpace x, Ord x, Ord y)
  => x
  -> Seq (Update x y) -- ^ Rightmost is recentmost.
  -> Candle x y
  -> Either (Err x y) (Candle x y)
update x = flip $ foldlM $ \s0 u -> do
    let s = x .+^ u.start
        e = x .+^ u.end
        o = u.open
        h = u.high
        l = u.low
        c = u.close
    when (s >= e) $ Left (Err_StartEnd s e)
    when (l > min o c || h < max o c) $ Left (Err_OHLC o h l c)
    pure $! s0 { info = Map.insert (s, e) (u.style, o, h, l, c) s0.info }

