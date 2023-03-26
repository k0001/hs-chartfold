{-# LANGUAGE StrictData #-}

module Chartfold.HLine {--}
 ( HLine(..)
 , update
 , Update(..)
 , initial
 , Config(..)
 , Style(..)
 , styleDefault
 ) --}
 where

import Data.Colour qualified as Co
import Data.Colour.Names qualified as Co
import Data.Kind
import Data.Sequence (Seq(..))
import Data.Text qualified as T

import Chartfold.Orphans ()

--------------------------------------------------------------------------------

data Style = Style
  { color  :: Co.AlphaColour Double
  , width  :: Double
  , dashes :: [Double]
  } deriving (Eq, Ord, Show)

styleDefault :: Style
styleDefault = Style
  { color  = Co.opaque Co.green
  , width  = 1
  , dashes = [1, 3]
  }

--

data HLine (y :: Type) = HLine
  { config :: Config
  , info   :: Maybe (Style, y)
    -- ^ We only ever display the most recent value, if any.
  } deriving stock (Eq, Ord, Show)

data Update y = Update
  { style :: Style
  , y     :: Maybe y
  } deriving stock (Eq, Ord, Show)

newtype Config = Config
  { title :: T.Text
  } deriving newtype (Eq, Ord)
    deriving stock (Show)

initial :: forall y. Config -> HLine y
initial d = HLine { config = d, info = Nothing }

update
  :: forall y
  .  Seq (Update y) -- ^ Rightmost is recentmost.
  -> HLine y
  -> HLine y
update us s = case us of
  _ :|> u -> s { info = fmap (u.style,) u.y }
  _ -> s

