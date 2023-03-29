{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Chartfold.XChart {--}
 ( XChart
 , pattern XChart
 , update
 , Update
 , Config
 , new
 ) --}
 where

import Data.AffineSpace (AffineSpace(..))
import Data.Constraint
import Data.Kind
import GHC.Show (appPrec, appPrec1)

import Chartfold.Chart qualified as Chart
import Chartfold.Constraint (Entails(..), Entails1)

--------------------------------------------------------------------------------

type role XChart nominal nominal representational
-- | A 'Chart.Chart' whose @y@ has been existentialized.
data XChart (s :: k) (x :: Type) (c :: Type -> Constraint) where
  MkXChart :: (c y, Ord y) => Chart.Chart s' x y -> XChart s x c
  -- ^ Note: It's easier to capture the 'Ord' constraint here than to deal with
  -- @'Entails1' c 'Ord'@ at usage sites. 'Ord' is fundamentally required by
  -- 'Chart.update' anyway, so it's OK to have it here.

pattern XChart :: () => (c y, Ord y) => Chart.Chart s' x y -> XChart s x c
pattern XChart a <- MkXChart a
{-# COMPLETE XChart #-}

instance HasField "config" (XChart s x c) (Config s x) where
  getField (XChart a) = Config a.config
  {-# INLINE getField #-}

instance (Eq x, Eq (Diff x)) => Eq (XChart s x c) where
  l == r = withXCharts l r (==)
  {-# INLINE (==) #-}

instance (Ord x, Ord (Diff x)) => Ord (XChart s x c) where
  compare l r = withXCharts l r compare
  {-# INLINE compare #-}
  l <= r = withXCharts l r (<=)
  {-# INLINE (<=) #-}

instance (Show x, Show (Diff x), Entails1 c Show)
  => Show (XChart s x c) where
  showsPrec n (XChart (a :: Chart.Chart s' x y)) =
    withDict (entails :: c y :- Show y) $
      showParen (n > appPrec) $
        showString "XChart " .
        showsPrec appPrec1 a

-- See 'update_' to understand why this is safe.
withXCharts
  :: forall s x c a
  .  XChart s x c
  -> XChart s x c
  -> (forall k (s' :: k) y
        .  (c y, Ord y)
        => Chart.Chart s' x y
        -> Chart.Chart s' x y
        -> a )
  -> a
withXCharts (XChart (l :: Chart.Chart (sl :: slk) x yl))
            (XChart (r :: Chart.Chart (sr :: srk) x yr))
  | HRefl <- unsafeCoerce HRefl :: sl :~~: sr
  , Refl  <- unsafeCoerce Refl  :: yl :~:  yr
  = \f -> f l r

--------------------------------------------------------------------------------

type role Update nominal nominal representational
data Update (s :: k) (x :: Type) (c :: Type -> Constraint) where
  Update :: (c y, Ord y) => Chart.Update s' x y -> Update s x c
  -- ^ Note: It's easier to capture the 'Ord' constraint here than to deal with
  -- @'Entails1' c 'Ord'@ at usage sites. 'Ord' is fundamentally required by
  -- 'Chart.update' anyway, so it's OK to have it here.

instance (Eq x) => Eq (Update s x c) where
  l == r = withUpdates l r (==)
  {-# INLINE (==) #-}

instance (Ord x) => Ord (Update s x c) where
  compare l r = withUpdates l r compare
  {-# INLINE compare #-}
  l <= r = withUpdates l r (<=)
  {-# INLINE (<=) #-}

instance (Show x, Show (Diff x), Entails1 c Show)
  => Show (Update s x c) where
  showsPrec n (Update (a :: Chart.Update s' x y)) =
    withDict (entails :: c y :- Show y) $
      showParen (n > appPrec) $
        showString "Update " .
        showsPrec appPrec1 a

-- | Notice that 'Update' is /not/ a 'Monoid' because there's
-- no way to construct a 'Chart' 'Chart.Update' without knowing @y@.
instance (Ord x) => Semigroup (Update s x c) where
  l <> r = withUpdates l r (\l' r' -> Update (l' <> r'))
  {-# INLINE (<>) #-}

-- See 'update_' to understand why this is safe.
withUpdates
  :: forall s x c a
  .  Update s x c
  -> Update s x c
  -> (forall k (s' :: k) y
        .  (c y, Ord y)
        => Chart.Update s' x y
        -> Chart.Update s' x y
        -> a )
  -> a
withUpdates (Update (l :: Chart.Update (sl :: slk) x yl))
            (Update (r :: Chart.Update (sr :: srk) x yr))
  | HRefl <- unsafeCoerce HRefl :: sl :~~: sr
  , Refl  <- unsafeCoerce Refl  :: yl :~:  yr
  = \f -> f l r

--------------------------------------------------------------------------------

type role Config nominal nominal
data Config (s :: k) (x :: Type) where
  Config :: Chart.Config s' x -> Config s x

instance (Eq (Diff x)) => Eq (Config s x) where
  l == r = withConfigs l r (==)
  {-# INLINE (==) #-}

instance (Ord (Diff x)) => Ord (Config s x) where
  compare l r = withConfigs l r compare
  {-# INLINE compare #-}
  l <= r = withConfigs l r (<=)
  {-# INLINE (<=) #-}

instance (Show (Diff x)) => Show (Config s x) where
  showsPrec n (Config u) = showsPrec n u

-- See 'update_' to understand why this is safe.
withConfigs
  :: forall s x a
  .  Config s x
  -> Config s x
  -> (forall k (s' :: k)
        .  Chart.Config s' x
        -> Chart.Config s' x
        -> a )
  -> a
withConfigs (Config (l :: Chart.Config (sl :: slk) x))
            (Config (r :: Chart.Config (sr :: srk) x))
  | HRefl <- unsafeCoerce HRefl :: sl :~~: sr
  = \f -> f l r

--------------------------------------------------------------------------------

new
  :: forall s' x y c a
  .  (c y, Ord y)
  => Chart.Chart s' x y
  -> (forall s
        .  XChart s x c
        -> (Chart.Update s' x y -> Update s x c)
        -> a )
  -> a
new c f = f (MkXChart c) Update

-- This function is safe for reasons similar to those in 'Chart.update'.
-- If the @s@ in 'Update' and 'XChart' agree, then @su ~~ sa@ and @yu ~ ya@.
-- The @s@ is always an uninstantiated type variable picked by 'new', and
-- there are no means other than 'new' to construct 'Update's or 'XChart's.
update :: forall s x c. (Ord x) => Update s x c -> XChart s x c -> XChart s x c
update (Update (u :: Chart.Update (su :: suk) x yu))
       (XChart (a :: Chart.Chart  (sa :: sak) x ya))
  | HRefl <- unsafeCoerce HRefl :: su :~~: sa
  , Refl  <- unsafeCoerce Refl  :: yu :~:  ya
  = MkXChart (Chart.update u a)

