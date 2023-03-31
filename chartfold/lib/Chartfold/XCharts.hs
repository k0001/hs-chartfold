{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Chartfold.XCharts {--}
 ( XCharts
 , Chartfold.XCharts.traverse
 , initial
 , update
 , Update
 , Config
 , configure
 , Configure
 , xchart
 ) --}
 where

import Data.AffineSpace (AffineSpace(..))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntMap.Merge.Strict qualified as IntMap
import Data.Kind
import GHC.Stack
import MTLPrelude

import Chartfold.Constraint
import Chartfold.XChart (XChart)
import Chartfold.XChart qualified as XChart

--------------------------------------------------------------------------------

{- Note [XCharts safety]

Multiple 'XChart's are kept inside an 'XChart'. Similarly, multiple
'XChart.Config's are kept inside a 'Config', and multiple 'XChart.Update's are
kept inside an 'Update'. Each of these contained elements are originally
indexed by some state tag introduced by 'XChart.configure', potentially
different from each other. To be able to store them all together in a same
'IntMap', we 'unsafeCoerce' all of those element state tags to 'Any'.

The creation of these 'IntMap's is fully controlled by 'configure', which also
introduces the state tag @s@ used by @'Config' s x c@, @'XCharts' s x c@ and
@'Update' s x c@. Whenever the @s@ on any of these types equals another one,
we can assume that any two elements sharing the same 'IntMap' can be
'unsafeCoerce'd to get their state tags to agree.

All of this works because 'configure' is the only way to influence the contents
of these 'IntMap's. There is no way to create values of type @'XChart' s x c@,
@'Config' s x c@ or @'Update' s x c@ independently from 'configure'. The only
exception is @'mempty' :: Update s x c@, but that creates an empty 'IntMap'
which for the purposes of the 'update' function, is fully compatible with the
one the one introduced by 'configure'.
-}

--------------------------------------------------------------------------------

-- The nominal role for @s@ is so that users can't 'coerce' 'XCharts'.
type role XCharts nominal nominal representational
-- | Multiple 'Chart.Chart's having the same @x@ axis, but potentially
-- different @y@ axes, all of them satisfying @c y@.,
newtype XCharts (s :: k) (x :: Type) (c :: Type -> Constraint) = XCharts
  { _xchart :: IntMap (XChart Any x c) -- ^ See Note [XCharts safety].
  }

deriving stock instance (Eq x, Eq (Diff x)) => Eq (XCharts s x c)
deriving stock instance (Ord x, Ord (Diff x)) => Ord (XCharts s x c)
deriving stock instance (Show x, Show (Diff x), Entails1 c Show)
  => Show (XCharts s x c)

-- | Traverse the 'XChart's in the 'XCharts', in the chronological order in
-- which they were added, from oldest to newest. It's not possible to know @s'@.
traverse
  :: forall s x c f a
  .  (Applicative f)
  => (forall k (s' :: k). XChart s' x c -> f a)
  -> XCharts s x c
  -> f [a]
traverse g (XCharts xs) = Prelude.traverse g (IntMap.elems xs)
{-# INLINE traverse #-}

--------------------------------------------------------------------------------

-- The nominal role for @s@ is so that users can't 'coerce' 'Update's.
type role Update nominal nominal representational
newtype Update (s :: Type) (x :: Type) (c :: Type -> Constraint) = Update
  { xchart :: IntMap (XChart.Update Any x c) -- ^ See Note [XCharts safety].
  }

instance (Ord x) => Semigroup (Update s x c) where
  Update ll <> Update rr = Update $ IntMap.merge
    IntMap.preserveMissing IntMap.preserveMissing
    (IntMap.zipWithMatched (\_ l r -> l <> r)) -- TODO make parallel, but only
                                               -- if more than one.
    ll rr
  {-# NOINLINE (<>) #-} -- ^ IntMap.merge is big and inlined.

instance (Ord x) => Monoid (Update s x c) where
  mempty = Update IntMap.empty

-- The nominal role for @s@ is so that users can't 'coerce' 'Config's.
type role Config nominal nominal representational
newtype Config (s :: k) (x :: Type) (c :: Type -> Constraint) = Config
  { xchart :: IntMap (XChart.Config Any x c) -- ^ See Note [XCharts safety]
  }

deriving stock instance Eq (Diff x) => Eq (Config s x c)
deriving stock instance Ord (Diff x) => Ord (Config s x c)
deriving stock instance Show (Diff x) => Show (Config s x c)

--------------------------------------------------------------------------------

initial :: forall s x c. Config s x c -> XCharts s x c
initial c = XCharts (XChart.initial <$> c.xchart)
{-# INLINE initial #-}

update
  :: forall s x c
  .  (Ord x)
  => Update s x c
  -> XCharts s x c
  -> XCharts s x c
update u c = g -- See Note [XCharts safety].
  where g :: HasCallStack => XCharts s x c -- HasCallStack for testing.
        g = either error id (update' u c)
{-# INLINE update #-}

update'
  :: forall s x c
  .  (Ord x)
  => Update s x c
  -> XCharts s x c
  -> Either String (XCharts s x c)
update' uu ss = XCharts <$>
  IntMap.mergeA
    (IntMap.traverseMissing (\k _ -> Left ("No XChart " <> show k)))
    (IntMap.preserveMissing)
    (IntMap.zipWithMatched (\_ l r -> XChart.update l r))
                            -- TODO make parallel, but only if more than one.
    uu.xchart
    ss._xchart
{-# NOINLINE update' #-} -- IntMap.mergeA is big and inlined.

--------------------------------------------------------------------------------

newtype Configure s x c a = Configure (State (Config s x c) a)
  deriving newtype (Functor, Applicative, Monad)

configure
  :: forall x c a
  .  (forall s. Configure s x c (Config s x c -> a))
  -- ^ Here you are expected to use 'xchart', possibly many times.
  -- See the example in the documentation.
  -> a
configure (Configure m) =
  case runState m (Config IntMap.empty) of
    (f, c) -> f c

xchart
  :: forall s' s x c
  .  XChart.Config s' x c
  -> Configure s x c (XChart.Update s' x c -> Update s x c)
xchart c = Configure $ state $ \cc ->
  let c' = unsafeCoerce c :: XChart.Config Any x c
      i = IntMap.size cc.xchart -- Safe as index because map never shrinks
  in ( \u -> let u' = unsafeCoerce u :: XChart.Update Any x c
             in  Update (IntMap.singleton i u')
     , Config (IntMap.insert i c' cc.xchart)
     )

