{-# LANGUAGE StrictData #-}

module Chartfold.HLine {--}
 ( -- * HLine
   HLine(config, points)
 , initial
 , update
   -- * Update
 , Update
 , add
 , del
 , clean
 , set
   -- * Config
 , Config(..)
 , configDefault
 , Style(..)
 , styleDefault
 ) --}
 where

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
  } deriving (Eq, Ord, Show)

styleDefault :: Style
styleDefault = Style
  { color  = Co.opaque Co.green
  , width  = 1
  , dashes = [1, 3]
  }

--------------------------------------------------------------------------------

newtype Config = Config
  { title :: T.Text
  } deriving newtype (Eq, Ord)
    deriving stock (Show)

configDefault :: T.Text -> Config
configDefault = Config

--------------------------------------------------------------------------------

data HLine y = HLine
  { config :: Config
  , points :: Map y Style
  } deriving stock (Eq, Ord, Show)

initial :: forall y. Config -> HLine y
initial c = HLine c Map.empty
{-# INLINE initial #-}

update :: forall y. Ord y => Update y -> HLine y -> HLine y
update (Update r u) (HLine c p) = HLine c $
  Map.mapMaybe id (u <> if r then mempty else fmap Just p)
{-# INLINE update #-}

--------------------------------------------------------------------------------

-- | This type represents 'Update's to an 'HLine'.
-- Construct with 'add', 'del', 'clean', 'set' or 'Monoid'.
data Update y = Update
  { reset :: Bool -- ^ Whether to reset previous changes.
  , points :: Map y (Maybe Style) -- ^ New points.
  } deriving stock (Eq, Ord, Show)

-- | @old '<>' new@.
instance Ord y => Semigroup (Update y) where
  l <> r = if r.reset then r
                      else Update r.reset (r.points <> l.points)

instance Ord y => Monoid (Update y) where
  mempty = Update{reset=False, points=mempty}

-- | Add an horizontal line at @y@, with style @s@.
-- Replaces the previous horizontal line at @y@, if any.
add :: forall y. Ord y => Style -> y -> Update y
add s y = Update{reset=False, points=Map.singleton y (Just s)}
{-# INLINE add #-}

-- | Delete the horizontal line at @y@, if any.
del :: forall y. Ord y => y -> Update y
del y = Update{reset=False, points=Map.singleton y Nothing}
{-# INLINE del #-}

-- | Delete all horizontal lines, if any.
clean :: forall y. Update y
clean = Update{reset=True, points=Map.empty}

-- | @'set' s y  '=='  'clean' <> 'add' s y@
set :: forall y. Style -> y -> Update y
set s y = Update{reset=True, points=Map.singleton y (Just s)}
{-# INLINE set #-}
