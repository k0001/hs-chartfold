{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Chartfold.Chart {--}
 ( Chart
 , initial
 , update
 , Update
 , Config
 , configure
 , Configure
 , candle
 , fill
 , hline
 , line
 , vline
 ) --}
 where

import Data.AffineSpace (AffineSpace(..))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntMap.Merge.Strict qualified as IntMap
import Data.Text qualified as T
import GHC.Stack (HasCallStack)
import MTLPrelude

import Chartfold.Candle (Candle)
import Chartfold.Candle qualified as Candle
import Chartfold.Fill (Fill)
import Chartfold.Fill qualified as Fill
import Chartfold.HLine (HLine)
import Chartfold.HLine qualified as HLine
import Chartfold.Line (Line)
import Chartfold.Line qualified as Line
import Chartfold.VLine (VLine)
import Chartfold.VLine qualified as VLine

--------------------------------------------------------------------------------

{- Note [Chart safety]

The only way to obtain a @'Config' s x@ is through 'configure', which choses
the @s@, and the only way to obtain a corresponding @'Chart' s x y@ is through
'initial'. 'configure' choses the keys for the 'IntMap's inside 'Config', and
'initial' preserves them. For this reason, we can make the assumption that
whenever state tag @s@ agrees, then the internal 'IntMap' keys will also agree
across @'Config' s x@ and @'Chart' s x y@, and they can be used match the
components of the chart. Keys in @'Update' s x y@, if present, will also
agree with those in @'Config' s x@ and @'Chart' s x y@.
-}

-- The nominal role for @s@ is so that users can't 'coerce' 'Chart'.
type role Chart nominal nominal nominal
-- | A 'Chart' is a collection of 'Fill's, 'Line's, 'HLine's, 'VLine's and
-- 'Candle's.
--
-- * The __only__ way to obtain a 'Chart' is through 'configure'.
--
-- * Check the 'HasField' instances for 'Chart'.
data Chart s x y = Chart
  { _config :: Config s x
  , _fill   :: IntMap (Fill x y)
  , _line   :: IntMap (Line x y)
  , _hline  :: IntMap (HLine y)
  , _vline  :: IntMap (VLine x)
  , _candle :: IntMap (Candle x y)
  }

-- | The 'Config' of this 'Chart'.
instance HasField "config" (Chart s x y) (Config s x) where
  getField = (._config)
  {-# INLINE getField #-}

-- | The 'Fill's in this 'Chart'. The rightmost is the most recently added.
instance HasField "fill" (Chart s x y) [Fill x y] where
  getField = IntMap.elems . (._fill)
  {-# INLINE getField #-}

-- | The 'Line's in this 'Chart'. The rightmost is the most recently added.
instance HasField "line" (Chart s x y) [Line x y] where
  getField = IntMap.elems . (._line)
  {-# INLINE getField #-}

-- | The 'HLine's in this 'Chart'. The rightmost is the most recently added.
instance HasField "hline" (Chart s x y) [HLine y] where
  getField = IntMap.elems . (._hline)
  {-# INLINE getField #-}

-- | The 'VLine's in this 'Chart'. The rightmost is the most recently added.
instance HasField "vline" (Chart s x y) [VLine x] where
  getField = IntMap.elems . (._vline)
  {-# INLINE getField #-}

-- | The 'Candle'in this 'Chart'. The rightmost is the most recently added.
instance HasField "candle" (Chart s x y) [Candle x y] where
  getField = IntMap.elems . (._candle)
  {-# INLINE getField #-}

deriving stock instance (Eq x, Eq (Diff x), Eq y) => Eq (Chart s x y)
deriving stock instance (Ord x, Ord (Diff x), Ord y) => Ord (Chart s x y)
deriving stock instance (Show x, Show (Diff x), Show y) => Show (Chart s x y)

initial :: forall s x y. Config s x -> Chart s x y
initial c = Chart
  { _config = c
  , _fill   = Fill.initial <$> c._fill
  , _line   = Line.initial <$> c._line
  , _hline  = HLine.initial <$> c._hline
  , _vline  = VLine.initial <$> c._vline
  , _candle = Candle.initial <$> c._candle
  }

-- | @'update' u c@ applies the 'Update' @u@ to 'Chart' @c@ in order to
-- obtain a modified 'Chart'.
--
-- ['Config' immutability law]
--
--     @∀ u c. ('update' u c)./config/  '=='  c./config/@
--
--     The 'Config' for a 'Chart', once chosen, can't be updated.
--
-- ['Update' identity law]
--
--     @∀ c. 'update' 'mempty' c  '=='  c@
--
--     'Update' is a 'Monoid' where 'mempty' means /update nothing/.
--
-- ['Update' composition law]
--
--     @∀ u v c. 'update' v ('update' u c)  '=='  'update' (u '<>' v)@
--
--     Keep in mind that @'update' (u '<>' v) c@ is usually faster than
--     @'update' v ('update' u c)@. Both achieve the same, though.
update
  :: forall s x y
  .  (Ord x, Ord y)
  => Update s x y
  -> Chart s x y
  -> Chart s x y
update u c = g
  -- You may be wondering why this is safe. Here's why. 'update_' returns
  -- 'Right' whenever all the 'IntMap' keys in the 'Update' are present
  -- in the 'Chart' 'IntMap's, as guaranteed by 'configure'. If it was possible
  -- to construct an 'Update' and a 'Chart' by means other than 'configure', then
  -- 'update' would be unsafe.  However, it's not possible to do so. There is
  -- 'escape', which allows us to obtain a @'Chart' () x y@. However, it's not
  -- possible to obtain a matching @'Update' () a y@ other than 'mempty', so
  -- that's not a problem.  In other words, the only practical possibilty is for
  -- the @s@ in @'Chart' s x y@ and @'Update' s x y@ to be the uninstantiated
  -- one introduced by 'configure', which guarantees that the 'IntMap' keys agree.
  where g :: HasCallStack => Chart s x y -- HasCallStack just in case.
        g = fromRight (error "impossible") (update' u c)

update'
  :: forall s x y
  .  (Ord x, Ord y)
  => Update s x y
  -> Chart s x y
  -> Either String (Chart s x y)
update' uu ss = Chart ss._config
    <$> g "Fill"   Fill.update   uu.fill   ss._fill
    <*> g "Line"   Line.update   uu.line   ss._line
    <*> g "HLine"  HLine.update  uu.hline  ss._hline
    <*> g "VLine"  VLine.update  uu.vline  ss._vline
    <*> g "Candle" Candle.update uu.candle ss._candle
  where
    g :: String
      -> (u -> s' -> s')
      -> IntMap u
      -> IntMap s'
      -> Either String (IntMap s')
    g e f = IntMap.mergeA
      (IntMap.traverseMissing (\k _ -> Left ("No " <> e <> " " <> show k)))
      (IntMap.preserveMissing)
      (IntMap.zipWithMatched (\_ l r -> f l r)) -- TODO: Make parallel, but only
                                                -- if more than one. Probably
                                                -- replace Chart's IntMaps with
                                                -- Vector.
    {-# INLINE g #-}
{-# NOINLINE update' #-} -- IntMap.mergeA is big and inlined.

--------------------------------------------------------------------------------

-- The nominal role for @s@ is so that users can't 'coerce' 'Update'.
type role Update nominal nominal nominal
-- | An update to a 'Chart'.
--
-- * Obtain through 'fill', 'line', 'hline', 'vline' or 'candle'.
--
-- * Compose with '<>'.
--
-- * Apply with 'update'.
data Update s x y = Update
  { fill   :: IntMap (Fill.Update x y)
  , line   :: IntMap (Line.Update x y)
  , hline  :: IntMap (HLine.Update y)
  , vline  :: IntMap (VLine.Update x)
  , candle :: IntMap (Candle.Update x y)
  }

-- | @old '<>' new@.
instance (Ord x, Ord y) => Semigroup (Update s x y) where
  l <> r = Update
    { fill   = IntMap.unionWith mappend l.fill   r.fill
    , line   = IntMap.unionWith mappend l.line   r.line
    , hline  = IntMap.unionWith mappend l.hline  r.hline
    , vline  = IntMap.unionWith mappend l.vline  r.vline
    , candle = IntMap.unionWith mappend l.candle r.candle
    }

-- | 'mempty' means /update nothing/.
instance (Ord x, Ord y) => Monoid (Update s x y) where
  mempty =  Update
    { fill   = mempty
    , line   = mempty
    , hline  = mempty
    , vline  = mempty
    , candle = mempty
    }

deriving stock instance (Eq x, Eq y) => Eq (Update s x y)
deriving stock instance (Ord x, Ord y) => Ord (Update s x y)
deriving stock instance (Show x, Show y) => Show (Update s x y)

--------------------------------------------------------------------------------

-- The nominal role for @s@ is so that users can't 'coerce' 'Config'.
type role Config nominal nominal
-- | The 'Config' for a 'Chart' and all its constituent parts.
--
-- * Can only be obtained through the /config/ field of a 'Chart'.
--
-- * Check the 'HasField' instances for 'Config'.
--
-- ['Config' immutability law]
--
--     @∀ u c. ('update' u c)./config/  '=='  c./config/@
--
--     The 'Config' for a 'Chart', once chosen, can't be updated.
data Config s x = Config
  { _title  :: T.Text
  , _fill   :: IntMap (Fill.Config x)
  , _line   :: IntMap (Line.Config x)
  , _hline  :: IntMap HLine.Config
  , _vline  :: IntMap (VLine.Config x)
  , _candle :: IntMap (Candle.Config x)
  }

-- | The title.
instance HasField "title" (Config s x) T.Text where
  getField = (._title)
-- | The @"Chartfold.Fill".@'Fill.Config's in this 'Config'. The rightmost is the most recently added.
instance HasField "fill" (Config s x) [Fill.Config x] where
  getField = IntMap.elems . (._fill)
-- | The @"Chartfold.Line".@'Line.Config's in this 'Config'. The rightmost is the most recently added.
instance HasField "line" (Config s x) [Line.Config x] where
  getField = IntMap.elems . (._line)
-- | The @"Chartfold.HLine".@'HLine.Config's in this 'Config'. The rightmost is the most recently added.
instance HasField "hline" (Config s x) [HLine.Config] where
  getField = IntMap.elems . (._hline)
-- | The @"Chartfold.VLine".@'VLine.Config's in this 'Config'. The rightmost is the most recently added.
instance HasField "vline" (Config s x) [VLine.Config x] where
  getField = IntMap.elems . (._vline)
-- | The @"Chartfold.Candle".@'Candle.Config's in this 'Config'. The rightmost is the most recently added.
instance HasField "candle" (Config s x) [Candle.Config x] where
  getField = IntMap.elems . (._candle)

deriving stock instance Eq (Diff x) => Eq (Config s x)
deriving stock instance Ord (Diff x) => Ord (Config s x)
deriving stock instance Show (Diff x) => Show (Config s x)

configDefault :: T.Text -> Config s x
configDefault title = Config
  { _title  = title
  , _line   = mempty
  , _hline  = mempty
  , _vline  = mempty
  , _fill   = mempty
  , _candle = mempty
  }

--------------------------------------------------------------------------------

-- | The configuration phase for a 'Chart' happens in 'Configure', a 'Monad'.
-- Refer to 'configure' for documentation and example.
--
-- * @__s__@ is an uninstantiated type chosen by 'configure'. Ignore it.
--
-- * @__x__@ is the same as in @'Config' x@.
--
-- * @__a__@ is the functorial output.
newtype Configure s x a = Configure (State (Config s x) a)
  deriving newtype (Functor, Applicative, Monad)

-- | @
-- 'configure' \"MyBeautifulChart\" $ do
--
--      -- Let's add two 'Line's to our 'Chart'. In return, we'll get two
--      -- functions /line1/ and /line2/ for updating those lines.
--      -- They allow us to convert "Chartfold.Line".'Line.Update's into
--      -- "Chartfold.Chart".'Update's targeting the correct 'Line'.
--
--      __line1__ :: "Chartfold.Line".'Line.Update' x y -> 'Update' __s__ x y
--            <- 'line' $ "Chartfold.Line".'Line.defaultConfig' \"MyFirstLine\"
--
--      __line2__ :: "Chartfold.Line".'Line.Update' x y -> 'Update' __s__ x y
--            <- 'line' $ "Chartfold.Line".'Line.defaultConfig' \"MySecondLine\"
--
--      -- Now that we are done configuring our 'Chart', let's return a
--      -- function that will have access not only to the /line1/ and
--      --- /line2/ we just created, but also to an /initial/ 'Chart'
--      -- as well as a function to /update/ it.
--
--      'pure' $ \\(__chart__ :: 'Config' __s__ x y) ->
--
--          -- Here you can do whatever you want, except help
--          -- /line1/, /line1/, /chart/ or anything that mentions
--          -- the type /s/ escape the scope of this 'configure' call.
--          -- The type-checker won't allow it.
-- @
configure
  :: forall x a
  .  T.Text  -- ^ Title.
  -> (forall s. Configure s x (Config s x -> a))
  -- ^ Here you are expected to use some of 'fill', 'line',
  -- 'hline', 'vline' and 'candle'. See the example in the
  -- documentation.
  -> a
configure t (Configure m) =
  case runState m (configDefault t) of
    (f, c) -> f c

-- | Add a 'Fill' to the 'Chart'.
fill
  :: forall s x y
  .  (Ord x, Ord y)
  => Fill.Config x
  -> Configure s x (Fill.Update x y -> Update s x y)
fill c = Configure $ state $ \cc ->
  let i = IntMap.size cc._fill
  in ( \u -> let Update{..} = mempty
              in Update{fill = IntMap.singleton i u, ..}
     , let Config{..} = cc
       in  Config{_fill = IntMap.insert i c cc._fill, ..}
     )

-- | Add a 'Line' to the 'Chart'.
line
  :: forall s x y
  .  (Ord x, Ord y)
  => Line.Config x
  -> Configure s x (Line.Update x y -> Update s x y)
line c = Configure $ state $ \cc ->
  let i = IntMap.size cc._line -- safe as index because map never shrinks
  in ( \u -> let Update{..} = mempty
              in Update{line = IntMap.singleton i u, ..}
     , let Config{..} = cc
       in  Config{_line = IntMap.insert i c cc._line, ..}
     )

-- | Add an 'HLine' to the 'Chart'.
hline
  :: forall s x y
  .  (Ord x, Ord y)
  => HLine.Config
  -> Configure s x (HLine.Update y -> Update s x y)
hline c = Configure $ state $ \cc ->
  let i = IntMap.size cc._hline -- safe as index because map never shrinks
  in ( \u -> let Update{..} = mempty
              in Update{hline = IntMap.singleton i u, ..}
     , let Config{..} = cc
       in  Config{_hline = IntMap.insert i c cc._hline, ..}
     )

-- | Add a 'VLine' to the 'Chart'.
vline
  :: forall s x y
  .  (Ord x, Ord y)
  => VLine.Config x
  -> Configure s x (VLine.Update x -> Update s x y)
vline c = Configure $ state $ \cc ->
  let i = IntMap.size cc._vline -- safe as index because map never shrinks
  in ( \u -> let Update{..} = mempty
              in Update{vline = IntMap.singleton i u, ..}
     , let Config{..} = cc
       in  Config{_vline = IntMap.insert i c cc._vline, ..}
     )

-- | Add a 'Candle' to the 'Chart'.
candle
  :: forall s x y
  .  (Ord x, Ord y)
  => Candle.Config x
  -> Configure s x (Candle.Update x y -> Update s x y)
candle c = Configure $ state $ \cc ->
  let i = IntMap.size cc._candle -- safe as index because map never shrinks
  in ( \u -> let Update{..} = mempty
              in Update{candle = IntMap.singleton i u, ..}
     , let Config{..} = cc
       in  Config{_candle = IntMap.insert i c cc._candle, ..}
     )

