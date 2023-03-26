{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Chartfold.Chart {--}
 ( Chart(..)
 , update
 , Update(..)
 , initial
 , Config(..)
 , configDefault
 , Err(..)
 ) --}
 where

import Data.AffineSpace (AffineSpace(..))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Sequence (Seq)
import Data.Text qualified as T

import Chartfold.Candle (Candle)
import Chartfold.Candle qualified as Candle
import Chartfold.Id (Id(..))
import Chartfold.Fill (Fill)
import Chartfold.Fill qualified as Fill
import Chartfold.HLine (HLine)
import Chartfold.HLine qualified as HLine
import Chartfold.Line (Line)
import Chartfold.Line qualified as Line
import Chartfold.VLine (VLine)
import Chartfold.VLine qualified as VLine

--------------------------------------------------------------------------------

data Chart x y = Chart
  { config :: Config x
  , line   :: IntMap (Line x y)
  , hline  :: IntMap (HLine y)
  , vline  :: IntMap (VLine x)
  , fill   :: IntMap (Fill x y)
  , candle :: IntMap (Candle x y)
  }

deriving stock instance (Eq (Diff x), Eq x, Eq y) => Eq (Chart x y)
deriving stock instance (Ord (Diff x), Ord x, Ord y) => Ord (Chart x y)
deriving stock instance (Show (Diff x), Show x, Show y) => Show (Chart x y)

data Update x y
  = Update_Line (Id (Line.Line x y)) (Line.Update y)
  | Update_HLine (Id (HLine.HLine y)) (HLine.Update y)
  | Update_VLine (Id (VLine.VLine x)) (VLine.Update x)
  | Update_Fill (Id (Fill.Fill x y)) (Fill.Update x y)
  | Update_Candle (Id (Candle.Candle x y)) (Candle.Update x y)

deriving stock instance (Eq (Diff x), Eq y) => Eq (Update x y)
deriving stock instance (Ord (Diff x), Ord y) => Ord (Update x y)
deriving stock instance (Show (Diff x), Show y) => Show (Update x y)

data Config x = Config
  { title  :: T.Text
  , line   :: IntMap (Line.Config x)
  , hline  :: IntMap HLine.Config
  , vline  :: IntMap VLine.Config
  , fill   :: IntMap Fill.Config
  , candle :: IntMap Candle.Config
  }

deriving stock instance Eq (Diff x) => Eq (Config x)
deriving stock instance Ord (Diff x) => Ord (Config x)
deriving stock instance Show (Diff x) => Show (Config x)

configDefault :: T.Text -> Config x
configDefault title = Config
  { title  = title
  , line   = mempty
  , hline  = mempty
  , vline  = mempty
  , fill   = mempty
  , candle = mempty
  }

data Err x y
  = Err_MissingLine (Id (Line x y))
  | Err_MissingHLine (Id (HLine y))
  | Err_MissingVLine (Id (VLine x))
  | Err_MissingFill (Id (Fill x y))
  | Err_MissingCandle (Id (Candle x y))
  | Err_Candle (Id (Candle x y)) (Candle.Err x y)
  deriving stock (Eq, Ord, Show)
  deriving anyclass (Exception)

initial :: forall x y. (Ord x) => Config x -> Chart x y
initial d = Chart
  { config  = d
  , line   = Line.initial   <$> d.line
  , hline  = HLine.initial  <$> d.hline
  , vline  = VLine.initial  <$> d.vline
  , fill   = Fill.initial   <$> d.fill
  , candle = Candle.initial <$> d.candle
  }

update
  :: (AffineSpace x, Ord x, Ord y)
  => x
  -> Seq (Update x y)
  -> Chart x y
  -> Either (Err x y) (Chart x y)
update = flip . foldlM . flip . update1

update1
  :: (AffineSpace x, Ord x, Ord y)
  => x
  -> Update x y
  -> Chart x y
  -> Either (Err x y) (Chart x y)
update1 x u0 s0 = case u0 of
    Update_Line i a -> do
      sa' <- mupdate i.un s0.line $ \case
        Nothing -> Left $ Err_MissingLine i
        Just sa -> pure $ Line.update x (pure a) sa
      pure $ case s0 of Chart{..} -> Chart{line=sa', ..}
    Update_HLine i a -> do
      sa' <- mupdate i.un s0.hline $ \case
        Nothing -> Left $ Err_MissingHLine i
        Just sa -> pure $ HLine.update (pure a) sa
      pure $ case s0 of Chart{..} -> Chart{hline=sa', ..}
    Update_VLine i a -> do
      sa' <- mupdate i.un s0.vline $ \case
        Nothing -> Left $ Err_MissingVLine i
        Just sa -> pure $ VLine.update x (pure a) sa
      pure $ case s0 of Chart{..} -> Chart{vline=sa', ..}
    Update_Fill i a -> do
      sa' <- mupdate i.un s0.fill $ \case
        Nothing -> Left $ Err_MissingFill i
        Just sa -> pure $ Fill.update x (pure a) sa
      pure $ case s0 of Chart{..} -> Chart{fill=sa', ..}
    Update_Candle i a -> do
      sa' <- mupdate i.un s0.candle $ \case
        Nothing -> Left $ Err_MissingCandle i
        Just sa -> first (Err_Candle i) $ Candle.update x (pure a) sa
      pure $ case s0 of Chart{..} -> Chart{candle=sa', ..}
  where
    mupdate :: forall f v. Functor f
            => Int -> IntMap v -> (Maybe v -> f v) -> f (IntMap v)
    mupdate i m f = IntMap.alterF (fmap Just . f) i m

{- What is this for?
instance (AffineSpace x, Ord x, Ord y)
  => ConfigAdd x (Chart x y) (Line.Line x y) where
  configAdd cb ca =
    let m  = ca.line
        i  = maybe 0 (succ . fst) $ IntMap.lookupMax m
        m' = IntMap.insert i cb m
        f  = \x ub a -> update x (Update_Line (Id i) <$> ub) a
    in (f, case ca of Config{..} -> Config{line = m', ..})
-}

