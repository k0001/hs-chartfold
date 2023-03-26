{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Chartfold.VLine
 ( VLine(..)
 , Config(Config, title)
 , Update(Update, style, x)
 , Err
 , Style(..)
 , style
 ) where

import Data.AffineSpace (AffineSpace(..))
import qualified Data.Colour as Co
import qualified Data.Colour.Names as Co
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

import Chartfold.Core

data Style = Style
  { color :: Co.AlphaColour Double
  , width :: Double
  , dashes :: [Double]
  } deriving (Eq, Ord, Show)

-- | Default 'Style'.
style :: Style
style = Style
  { color = Co.opaque Co.blue
  , width = 1
  , dashes = [1, 3]
  }

data VLine x y = VLine
  { config :: Config (VLine x y)
  , info :: Map Style (Set x)
  } deriving stock (Eq, Ord, Show)

instance (AffineSpace x, Ord x) => Element x (VLine x y) where
  data Update (VLine x y) = Update
    { style :: Style
    , x :: Diff x
    }

  newtype Config (VLine x y) = Config
    { title :: T.Text
    } deriving stock (Eq, Ord, Show)

  data Err (VLine x y)
    deriving stock (Eq, Ord, Show)
    deriving anyclass (Exception)

  element d = VLine { config = d, info = mempty }

  update x us s = Right $ s
    { info = foldl' (\m u -> Map.insertWith
                               mappend
                               u.style
                               (Set.singleton $! x .+^ u.x)
                               m)
                    s.info
                    us
    }

deriving stock instance Eq (Diff x) => Eq (Update (VLine x y))
deriving stock instance Ord (Diff x) => Ord (Update (VLine x y))
deriving stock instance Show (Diff x) => Show (Update (VLine x y))

