{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Chartfold.VLine {--}
 ( -- * VLine
   VLine
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

import Data.AdditiveGroup (AdditiveGroup(zeroV))
import Data.AffineSpace (AffineSpace(Diff))
import Data.Colour qualified as Co
import Data.Colour.Names qualified as Co
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Chartfold.Orphans ()

--------------------------------------------------------------------------------

data Style = Style
  { color :: Co.AlphaColour Double
  , width :: Double
  , dashes :: [Double]
  } deriving (Eq, Ord, Show)

styleDefault :: Style
styleDefault = Style
  { color = Co.opaque Co.blue
  , width = 1
  , dashes = [1, 3]
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

data VLine x = VLine
  { _config :: Config x
  , _points :: Map x Style
  }

instance HasField "config" (VLine x) (Config x) where
  getField = (._config)
instance HasField "points" (VLine x) (Map x Style) where
  getField = (._points)

deriving stock instance (Eq x, Eq (Diff x)) => Eq (VLine x)
deriving stock instance (Ord x, Ord (Diff x)) => Ord (VLine x)
deriving stock instance (Show x, Show (Diff x)) => Show (VLine x)

initial :: forall x. Config x -> VLine x
initial c = VLine c Map.empty
{-# INLINE initial #-}

update :: forall x. Ord x => Update x -> VLine x -> VLine x
update (Update r u) (VLine c p) = VLine c $
  Map.mapMaybe id (u <> if r then mempty else fmap Just p)
{-# INLINE update #-}

--------------------------------------------------------------------------------

-- | This type represents 'Update's to a 'VLine'.
-- Construct with 'add', 'del', 'clean', 'set' or 'Monoid'.
data Update x = Update
  { reset :: Bool -- ^ Whether to reset previous changes.
  , points :: Map x (Maybe Style) -- ^ New points.
  } deriving stock (Eq, Ord, Show)

-- | @old '<>' new@.
instance Ord x => Semigroup (Update x) where
  l <> r = if r.reset then r
                      else Update r.reset (r.points <> l.points)

instance Ord x => Monoid (Update x) where
  mempty = Update{reset=False, points=mempty}

-- | Add an vertical line at @x@, with style @s@.
-- Replaces the previous vertical line at @x@, if any.
add :: forall x. Ord x => Style -> x -> Update x
add s x = Update{reset=False, points=Map.singleton x (Just s)}
{-# INLINE add #-}

-- | Delete the vertical line at @x@, if any.
del :: forall x. Ord x => x -> Update x
del x = Update{reset=False, points=Map.singleton x Nothing}
{-# INLINE del #-}

-- | Delete all vertical lines, if any.
clean :: forall x. Update x
clean = Update{reset=True, points=Map.empty}

-- | @'set' s x  '=='  'clean' <> 'add' s x@
set :: forall x. Style -> x -> Update x
set s x = Update{reset=True, points=Map.singleton x (Just s)}
{-# INLINE set #-}

