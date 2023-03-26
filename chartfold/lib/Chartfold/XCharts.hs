{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Chartfold.XCharts
 ( XCharts(..)
 , Config(Config)
 , Update(Update)
 , Err(ErrXChart_UnknownY, ErrXChart_Chart)
 ) where

import Control.Lens (ifor)
import Data.AffineSpace (AffineSpace(..))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Text qualified as T

import Chartfold.Core
import Chartfold.XChart qualified as XChart
import Chartfold.Constraint

--

data XCharts x c = XCharts
  { charts :: IntMap (XChart.XChart x c)
  }

deriving stock instance Eq (XChart.XChart x c) => Eq (XCharts x c)
deriving stock instance Show (XChart.XChart x c) => Show (XCharts x c)


deriving stock instance Eq (Update (XChart.XChart x c))
  => Eq (Update (XCharts x c))
deriving stock instance Show (Update (XChart.XChart x c))
  => Show (Update (XCharts x c))


deriving stock instance Eq (Config (XChart.XChart x c))
  => Eq (Config (XCharts x c))
deriving stock instance Show (Config (XChart.XChart x c))
  => Show (Config (XCharts x c))

deriving stock instance Eq (Err (XChart.XChart x c))
  => Eq (Err (XCharts x c))
deriving stock instance Show (Err (XChart.XChart x c))
  => Show (Err (XCharts x c))
deriving anyclass instance
  (Exception (Err (XChart.XChart x c)), Typeable x, Typeable c)
  => Exception (Err (XCharts x c))

instance
  ( AffineSpace x
  , Ord x
  , Entails1 c Ord
  , Entails1 c Typeable
  ) => Element x (XCharts x c) where

  data instance Update (XCharts x c) = Update
    { charts :: IntMap (Update (XChart.XChart x c))
    }

  data instance Config (XCharts x c) = Config
    { title  :: T.Text
    , charts :: IntMap (Config (XChart.XChart x c))
    }

  data instance Err (XCharts x c)
    = Err_MissingIdXChart (Id (XChart.XChart x c))
    | Err_XChart (Id (XChart.XChart x c)) (Err (XChart.XChart x c))

  element d = XCharts { charts = element <$> d.charts }

  update x = flip $ foldlM $ \s u -> do
    cs <- ifor u.charts $ \i b -> do
      c <- case IntMap.lookup i s.charts of
        Just xc -> Right xc
        Nothing -> Left $ Err_MissingIdXChart (Id i)
      first (Err_XChart (Id i)) $ update x (pure b) c
    pure $ XCharts{charts = cs}
