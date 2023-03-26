{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Chartfold.XCharts {--}
 ( XCharts(..)
 , update
 , Update(..)
 , initial
 , Config(..)
 , Err(..)
 ) --}
 where

import Control.Lens (ifor)
import Data.AffineSpace (AffineSpace(..))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Kind
import Data.Sequence (Seq)
import Data.Text qualified as T

import Chartfold.Chart qualified as Chart
import Chartfold.Constraint
import Chartfold.Id (Id(..))
import Chartfold.XChart (XChart)
import Chartfold.XChart qualified as XChart

--------------------------------------------------------------------------------

{-
TODO: Do we need this? It's like 'XCharts' but each 'XChart' @c@ is independent.
data XChartLR x cl cr = XChartLR (XChart x cl) (XChart x cr)
-}

-- | Multiple 'Chart.Chart's having the same @x@ axis, but potentially
-- different @y@ axis satisfying @c y@.,
data XCharts (x :: Type) (c :: Type -> Constraint)= XCharts
  { xchart :: IntMap (XChart x c)
  }

deriving stock instance Eq (XChart x c) => Eq (XCharts x c)
deriving stock instance Ord (XChart x c) => Ord (XCharts x c)
deriving stock instance Show (XChart x c) => Show (XCharts x c)


data Update (x :: Type) (c :: Type -> Constraint) = Update
  { xchart :: IntMap (XChart.Update x c)
  }

deriving stock instance Eq (XChart.Update x c) => Eq (Update x c)
deriving stock instance Ord (XChart.Update x c) => Ord (Update x c)
deriving stock instance Show (XChart.Update x c) => Show (Update x c)

data Config (x :: Type) = Config
  { title  :: T.Text
  , xchart :: IntMap (Chart.Config x)
  }

deriving stock instance Eq (Chart.Config x) => Eq (Config x)
deriving stock instance Ord (Chart.Config x) => Ord (Config x)
deriving stock instance Show (Chart.Config x) => Show (Config x)

data Err x c
  = Err_MissingXChart (Id (XChart x c))
  | Err_XChart (Id (XChart x c)) (XChart.Err x c)

deriving stock instance Eq (XChart.Err x c) => Eq (Err x c)
deriving stock instance Ord (XChart.Err x c) => Ord (Err x c)
deriving stock instance Show (XChart.Err x c) => Show (Err x c)
deriving anyclass instance (Typeable (Err x c), Show (Err x c))
  => Exception (Err x c)

initial :: forall x y c. (Ord x, c y) => Proxy y -> Config x -> XCharts x c
initial p d = XCharts { xchart = XChart.initial p <$> d.xchart }

update
  :: forall x c
  .  ( AffineSpace x
     , Ord x
     , Entails1 c Ord
     , Entails1 c Typeable )
  => x
  -> Seq (Update x c)
  -> XCharts x c
  -> Either (Err x c) (XCharts x c)
update x = flip $ foldlM $ \s u -> do
  cs <- ifor u.xchart $ \i b -> do
    c <- case IntMap.lookup i s.xchart of
      Just xc -> Right xc
      Nothing -> Left $ Err_MissingXChart (Id i)
    first (Err_XChart (Id i)) $ XChart.update x (pure b) c
  pure $ XCharts{xchart = cs}

