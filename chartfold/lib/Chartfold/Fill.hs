{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Chartfold.Fill
 ( Fill(..)
 , Config(Config, title)
 , Update(Update, style, x, y)
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
  } deriving stock (Eq, Ord, Show)

-- | Default 'Style'.
style :: Style
style = Style
  { color = Co.withOpacity Co.blue 0.6
  }

data Fill x y = Fill
  { config :: Config (Fill x y)
  , info   :: Map Style (Map x (Set (y, y)))
  } deriving stock (Eq, Ord, Show)

instance (AffineSpace x, Ord x, Ord y) => Element x (Fill x y) where
  data instance Update (Fill x y) = Update
    { style :: Style
    , x     :: Diff x
    , y     :: (y, y)
    }

  newtype instance Config (Fill x y) = Config
    { title :: T.Text
    } deriving stock (Eq, Ord, Show)

  data instance Err (Fill x y)
    deriving stock (Eq, Ord, Show)
    deriving anyclass (Exception)

  element d = Fill { config = d , info = mempty }

  update x us s = Right $! s
    { info = foldl'
       (\m u -> let !x' = x .+^ u.x
                    !ys = Set.singleton (uncurry min u.y, uncurry max u.y)
                in Map.alter (Just . maybe (Map.singleton x' ys)
                                           (Map.insertWith mappend x' ys))
                             u.style
                             m)
       s.info
       us
    }

deriving stock instance (Eq (Diff x), Eq y) => Eq (Update (Fill x y))
deriving stock instance (Ord (Diff x), Ord y) => Ord (Update (Fill x y))
deriving stock instance (Show (Diff x), Show y) => Show (Update (Fill x y))

