{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Chartfold.Candle
 ( Candle(..)
 , Config(Config, title)
 , Update(Update, style, start, end, open, high, low, close)
 , Err(Err_StartEnd, Err_OHLC)
 , Style(..)
 , style
 ) where

import Data.AffineSpace (AffineSpace(..))
import qualified Data.Colour as Co
import qualified Data.Colour.Names as Co
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Chartfold.Core

data Style = Style
  { fillColor  :: Co.AlphaColour Double
  , lineColor  :: Co.AlphaColour Double
  , lineWidth  :: Double
  , lineDashes :: [Double]
  } deriving (Eq, Ord, Show)

-- | Default 'Style'.
style :: Style
style = Style
  { fillColor  = Co.opaque Co.white
  , lineColor  = Co.opaque Co.black
  , lineWidth  = 1
  , lineDashes = []
  }

data Candle x y = Candle
  { config :: Config (Candle x y)
  , info   :: Map (x, x) (Style, y, y, y, y)
    -- ^ (start, end) -> (_, _, open, high, low, close).
    --
    -- We only keep the most recently set candle.
  } deriving stock (Eq, Ord, Show)

instance (AffineSpace x, Ord x, Ord y) => Element x (Candle x y) where
  data instance Update (Candle x y) = Update
    { style :: Style
    , start :: Diff x
    , end   :: Diff x
    , open  :: y
    , high  :: y
    , low   :: y
    , close :: y
    }

  data instance Config (Candle x y) = Config
    { title :: T.Text
    } deriving stock (Eq, Ord, Show)

  data instance Err (Candle x y)
    = Err_StartEnd x x
      -- ^ If candle start time isn't before end time.
      -- /Start, end./
    | Err_OHLC y y y y
      -- ^ If low > min(open, close) or high < max(open, close).
      -- /Open, high, low, close./
    deriving stock (Eq, Ord, Show)
    deriving anyclass (Exception)

  element d = Candle { config = d , info = mempty }

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

deriving stock instance (Eq (Diff x), Eq y) => Eq (Update (Candle x y))
deriving stock instance (Ord (Diff x), Ord y) => Ord (Update (Candle x y))
deriving stock instance (Show (Diff x), Show y) => Show (Update (Candle x y))

