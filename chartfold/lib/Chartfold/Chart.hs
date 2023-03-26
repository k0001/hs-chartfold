{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Chartfold.Chart
 ( Chart(..)
 , Config(Config, title, lines, hLines, vLines, fills, candles)
 , defaultConfig
 , Update(UpdateChartLine,
          UpdateChartHLine,
          UpdateChartVLine,
          UpdateChartFill,
          UpdateChartCandle)
  , Err(Err_MissingIdLine,
        Err_MissingIdHLine,
        Err_MissingIdVLine,
        Err_MissingIdFill,
        Err_MissingIdCandle,
        Err_Line,
        Err_HLine,
        Err_VLine,
        Err_Fill,
        Err_Candle)
 ) where

import Data.AffineSpace (AffineSpace(..))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Text as T
import Prelude hiding (lines)

import Chartfold.Core
import qualified Chartfold.Candle as Candle
import qualified Chartfold.Fill as Fill
import qualified Chartfold.HLine as HLine
import qualified Chartfold.Line as Line
import qualified Chartfold.VLine as VLine

--

data Chart x y = Chart
  { config  :: Config (Chart x y)
  , lines   :: IntMap (Line.Line x y)
  , hLines  :: IntMap (HLine.HLine x y)
  , vLines  :: IntMap (VLine.VLine x y)
  , fills   :: IntMap (Fill.Fill x y)
  , candles :: IntMap (Candle.Candle x y)
  }

instance (AffineSpace x, Ord x, Ord y) => Element x (Chart x y) where
  data instance Update (Chart x y)
    = UpdateChartLine (Id (Line.Line x y)) (Update (Line.Line x y))
    | UpdateChartHLine (Id (HLine.HLine x y)) (Update (HLine.HLine x y))
    | UpdateChartVLine (Id (VLine.VLine x y)) (Update (VLine.VLine x y))
    | UpdateChartFill (Id (Fill.Fill x y)) (Update (Fill.Fill x y))
    | UpdateChartCandle (Id (Candle.Candle x y)) (Update (Candle.Candle x y))

  data instance Config (Chart x y) = Config
    { title   :: T.Text
    , lines   :: IntMap (Config (Line.Line x y))
    , hLines  :: IntMap (Config (HLine.HLine x y))
    , vLines  :: IntMap (Config (VLine.VLine x y))
    , fills   :: IntMap (Config (Fill.Fill x y))
    , candles :: IntMap (Config (Candle.Candle x y))
    }

  data instance Err (Chart x y)
    = Err_MissingIdLine (Id (Line.Line x y))
    | Err_MissingIdHLine (Id (HLine.HLine x y))
    | Err_MissingIdVLine (Id (VLine.VLine x y))
    | Err_MissingIdFill (Id (Fill.Fill x y))
    | Err_MissingIdCandle (Id (Candle.Candle x y))
    | Err_Line (Id (Line.Line x y)) (Err (Line.Line x y))
    | Err_HLine (Id (HLine.HLine x y)) (Err (HLine.HLine x y))
    | Err_VLine (Id (VLine.VLine x y)) (Err (VLine.VLine x y))
    | Err_Fill (Id (Fill.Fill x y)) (Err (Fill.Fill x y))
    | Err_Candle (Id (Candle.Candle x y)) (Err (Candle.Candle x y))
    deriving stock (Eq, Ord, Show)
    deriving anyclass (Exception)

  element d = Chart
    { config  = d
    , lines   = element <$> d.lines
    , hLines  = element <$> d.hLines
    , vLines  = element <$> d.vLines
    , fills   = element <$> d.fills
    , candles = element <$> d.candles
    }

  update = flip . foldlM . flip . update1

update1 :: (AffineSpace x, Ord x, Ord y)
        => x -> Update (Chart x y) -> Chart x y
        -> Either (Err (Chart x y)) (Chart x y)
update1 x u0 s0 = case u0 of
    UpdateChartLine i a -> do
      sa' <- mupdate i.un s0.lines $ \case
        Nothing -> Left $ Err_MissingIdLine i
        Just sa -> first (Err_Line i) $ update x (pure a) sa
      pure $ case s0 of Chart{..} -> Chart{lines = sa', ..}
    UpdateChartHLine i a -> do
      sa' <- mupdate i.un s0.hLines $ \case
        Nothing -> Left $ Err_MissingIdHLine i
        Just sa -> first (Err_HLine i) $ update x (pure a) sa
      pure $ case s0 of Chart{..} -> Chart{hLines = sa', ..}
    UpdateChartVLine i a -> do
      sa' <- mupdate i.un s0.vLines $ \case
        Nothing -> Left $ Err_MissingIdVLine i
        Just sa -> first (Err_VLine i) $ update x (pure a) sa
      pure $ case s0 of Chart{..} -> Chart{vLines = sa', ..}
    UpdateChartFill i a -> do
      sa' <- mupdate i.un s0.fills $ \case
        Nothing -> Left $ Err_MissingIdFill i
        Just sa -> first (Err_Fill i) $ update x (pure a) sa
      pure $ case s0 of Chart{..} -> Chart{fills = sa', ..}
    UpdateChartCandle i a -> do
      sa' <- mupdate i.un s0.candles $ \case
        Nothing -> Left $ Err_MissingIdCandle i
        Just sa -> first (Err_Candle i) $ update x (pure a) sa
      pure $ case s0 of Chart{..} -> Chart{candles = sa', ..}
  where
    mupdate :: forall f v. Functor f
            => Int -> IntMap v -> (Maybe v -> f v) -> f (IntMap v)
    mupdate i m f = IntMap.alterF (fmap Just . f) i m

deriving stock instance (Eq (Diff x), Eq x, Eq y) => Eq (Chart x y)
deriving stock instance (Ord (Diff x), Ord x, Ord y) => Ord (Chart x y)
deriving stock instance (Show (Diff x), Show x, Show y) => Show (Chart x y)

deriving stock instance (Eq (Diff x), Eq y) => Eq (Update (Chart x y))
deriving stock instance (Ord (Diff x), Ord y) => Ord (Update (Chart x y))
deriving stock instance (Show (Diff x), Show y) => Show (Update (Chart x y))

deriving stock instance Eq (Diff x) => Eq (Config (Chart x y))
deriving stock instance Ord (Diff x) => Ord (Config (Chart x y))
deriving stock instance Show (Diff x) => Show (Config (Chart x y))

instance (AffineSpace x, Ord x, Ord y)
  => ConfigAdd x (Chart x y) (Line.Line x y) where
  configAdd cb ca =
    let m  = ca.lines
        i  = maybe 0 (succ . fst) $ IntMap.lookupMax m
        m' = IntMap.insert i cb m
        f  = \x ub a -> update x (UpdateChartLine (Id i) <$> ub) a
    in (f, case ca of Config{..} -> Config{lines = m', ..})

defaultConfig :: T.Text -> Config (Chart x y)
defaultConfig title = Config
  { title   = title
  , lines   = mempty
  , hLines  = mempty
  , vLines  = mempty
  , fills   = mempty
  , candles = mempty
  }

