{-# LANGUAGE StrictData #-}

module Chartfold.Extra {--}
 ( OHLC
 , ohlc
 , Interval
 , interval
 ) --}
 where

import Chartfold.Orphans ()

--------------------------------------------------------------------------------

-- | Open, high, low, close.
--
-- * Construct with 'ohlc'.
--
-- * Access fields through 'HasField'.
data OHLC y = OHLC
  { _open  :: y
  , _high  :: y
  , _low   :: y
  , _close :: y
  } deriving stock (Eq, Ord, Show, Functor)

instance HasField "open" (OHLC y) y where
  getField = (._open)
  {-# INLINE getField #-}
instance HasField "high" (OHLC y) y where
  getField = (._high)
  {-# INLINE getField #-}
instance HasField "low" (OHLC y) y where
  getField = (._low)
  {-# INLINE getField #-}
instance HasField "close" (OHLC y) y where
  getField = (._close)
  {-# INLINE getField #-}

ohlc
  :: forall y
  .  Ord y
  => y -- ^ Open
  -> y -- ^ High
  -> y -- ^ Low
  -> y -- ^ Close
  -> Maybe (OHLC y)
ohlc o h l c = do
  guard (l <= min o c && max o c <= h)
  pure OHLC{_open=o, _high=h, _low=l, _close=c}
{-# INLINE ohlc #-}

--------------------------------------------------------------------------------

-- ^ A /closed/ interval /[@start@, @end@]/.
--
-- * Construct with 'interval'.
--
-- * Access fields through 'HasField'.
data Interval a = Interval
  { _start :: a
  , _end   :: a
  } deriving stock (Eq, Ord, Show, Functor)

instance HasField "start" (Interval a) a where
  getField = (._start)
  {-# INLINE getField #-}
instance HasField "end" (Interval a) a where
  getField = (._end)
  {-# INLINE getField #-}

interval
  :: forall x
  .  Ord x
  => x -- ^ 'start'. At most 'end'.
  -> x -- ^ 'end'. At least 'start'.
  -> Maybe (Interval x)
interval s e = do
  guard (s <= e)
  pure Interval{_start=s, _end=e}
{-# INLINE interval #-}

