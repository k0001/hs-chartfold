{-# LANGUAGE StrictData #-}

module Chartfold.HLine
 ( HLine(..)
 , Config(Config, title)
 , Update(Update, style, y)
 , Err
 , Style(..)
 , style
 ) where

import Data.Colour qualified as Co
import Data.Colour.Names qualified as Co
import Data.Kind
import Data.Sequence qualified as Seq
import Data.Text qualified as T

import Chartfold.Core

--

data Style = Style
  { color  :: Co.AlphaColour Double
  , width  :: Double
  , dashes :: [Double]
  } deriving (Eq, Ord, Show)

-- | Default 'Style'.
style :: Style
style = Style
  { color  = Co.opaque Co.green
  , width  = 1
  , dashes = [1, 3]
  }

--
data HLine (x :: Type) (y :: Type) = HLine
  { config :: Config (HLine x y)
  , info   :: Maybe (Style, y)
    -- ^ We only ever display the most recent value, if any.
  } deriving stock (Eq, Ord, Show)

instance Element x (HLine x y) where
  data instance Update (HLine x y) = Update
    { style :: Style
    , y     :: Maybe y
    } deriving stock (Eq, Ord, Show)

  newtype instance Config (HLine x y) = Config
    { title :: T.Text
    } deriving stock (Eq, Ord, Show)

  data instance Err (HLine x y)
    deriving stock (Eq, Ord, Show)
    deriving anyclass (Exception)

  element d = HLine { config = d, info = Nothing }

  update _ us s = case us of
    Seq.Empty -> Right s
    _ Seq.:|> u -> Right $! s { info = fmap (u.style,) u.y }

