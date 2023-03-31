{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Chartfold.XChart {--}
 ( XChart
 , pattern XChart
 , initial
 , update
 , Update
 , Config
 , configure
 ) --}
 where

import Data.AffineSpace (AffineSpace(..))
import Data.Constraint
import Data.Kind
import GHC.Show (appPrec, appPrec1)

import Chartfold.Chart (Chart)
import Chartfold.Chart qualified as Chart
import Chartfold.Constraint (Entails(..), Entails1)

--------------------------------------------------------------------------------

-- The nominal role for @s@ is so that users can't 'coerce' 'XChart's.
type role XChart nominal nominal representational
-- | A 'Chart' whose @y@ has been existentialized.
data XChart (s :: k) (x :: Type) (c :: Type -> Constraint) where
  MkXChart :: (c y, Ord y) => Chart s' x y -> XChart s x c
  -- ^ Note: It's easier to capture the 'Ord' constraint here than to deal with
  -- @'Entails1' c 'Ord'@ at usage sites. 'Ord' is fundamentally required by
  -- 'Chart.update' anyway, so it's OK to have it here.

-- | Pattern-match on an @'XChart' s x c@ to reveal the underlying
-- @'Chart' s' x y@.
--
-- Notice that the uncovered @s'@ is unknown.
pattern XChart :: () => (c y, Ord y) => Chart s' x y -> XChart s x c
pattern XChart a <- MkXChart a
{-# COMPLETE XChart #-}

-- Do we want to export this?
--
-- instance HasField "config" (XChart s x c) (Config s x) where
--   getField (XChart a) = Config a.config
--   {-# INLINE getField #-}

instance (Eq x, Eq (Diff x)) => Eq (XChart s x c) where
  l == r = withXCharts l r (==)
  {-# INLINE (==) #-}

instance (Ord x, Ord (Diff x)) => Ord (XChart s x c) where
  compare l r = withXCharts l r compare
  {-# INLINE compare #-}
  l <= r = withXCharts l r (<=)
  {-# INLINE (<=) #-}

instance (Show x, Show (Diff x), Entails1 c Show) => Show (XChart s x c) where
  showsPrec n (XChart (a :: Chart s' x y)) =
    withDict (entails :: c y :- Show y) $
      showParen (n > appPrec) $
        showString "XChart " .
        showsPrec appPrec1 a

-- See 'update' to understand why this is safe.
withXCharts
  :: forall s x c a
  .  XChart s x c
  -> XChart s x c
  -> (forall k (s' :: k) y
        .  (c y, Ord y)
        => Chart s' x y
        -> Chart s' x y
        -> a )
  -> a
withXCharts (XChart (l :: Chart (sl :: slk) x yl))
            (XChart (r :: Chart (sr :: srk) x yr)) f
  | HRefl <- unsafeCoerce HRefl :: sl :~~: sr
  , Refl  <- unsafeCoerce Refl  :: yl :~:  yr
  = f l r

--------------------------------------------------------------------------------

initial :: forall s x c. Config s x c -> XChart s x c
initial (Config (_ :: Proxy y) (c :: Chart.Config s' x)) =
  MkXChart (Chart.initial c :: Chart s' x y)
{-# INLINE initial #-}

--------------------------------------------------------------------------------

-- The nominal role for @s@ is so that users can't 'coerce' 'WrappedUpdate's.
-- This is moot, though, since this type is never exported.
type role WrappedUpdate nominal nominal representational
-- | See 'Update'.
data WrappedUpdate (s :: k) (x :: Type) (c :: Type -> Constraint) where
  WrappedUpdate :: (c y, Ord y) => Chart.Update s' x y -> WrappedUpdate s x c
  -- ^ Note: It's easier to capture the 'Ord' constraint here than to deal with
  -- @'Entails1' c 'Ord'@ at usage sites. 'Ord' is fundamentally required by
  -- 'Chart.update' anyway, so it's OK.

-- | See 'update' to understand why this is safe.
withWrappedUpdates
  :: forall s x c a
  .  WrappedUpdate s x c
  -> WrappedUpdate s x c
  -> (forall k (s' :: k) y
        .  (c y, Ord y)
        => Chart.Update s' x y
        -> Chart.Update s' x y
        -> a )
  -> a
withWrappedUpdates (WrappedUpdate (l :: Chart.Update (sl :: slk) x yl))
                   (WrappedUpdate (r :: Chart.Update (sr :: srk) x yr)) f
  | HRefl <- unsafeCoerce HRefl :: sl :~~: sr
  , Refl  <- unsafeCoerce Refl  :: yl :~:  yr
  = f l r

instance (Eq x) => Eq (WrappedUpdate s x c) where
  l == r = withWrappedUpdates l r (==)
  {-# INLINE (==) #-}

instance (Ord x) => Ord (WrappedUpdate s x c) where
  l `compare` r = withWrappedUpdates l r compare
  {-# INLINE compare #-}
  l <= r = withWrappedUpdates l r (<=)
  {-# INLINE (<=) #-}

instance (Show x, Entails1 c Show) => Show (WrappedUpdate s x c) where
  showsPrec n (WrappedUpdate (a :: Chart.Update s' x y)) =
    withDict (entails :: c y :- Show y) $ showsPrec n a

-- | Notice that 'WrappedUpdate' is /not/ a 'Monoid' because there's
-- no way to construct a 'Chart' 'Chart.Update' without knowing @y@.
-- That's why we have 'Update' with its extra 'NoUpdate' constructor.
instance (Ord x) => Semigroup (WrappedUpdate s x c) where
  l <> r = withWrappedUpdates l r (\l' r' -> WrappedUpdate (l' <> r'))
  {-# INLINE (<>) #-}

--------------------------------------------------------------------------------

-- The nominal role for @s@ is so that users can't 'coerce' 'Update's.
type role Update nominal nominal representational
-- | Wrapper around a @"Chartfold.Chart".'Chart.Update'@ that hides its @y@.
data Update (s :: k) (x :: Type) (c :: Type -> Constraint) where
  NoUpdate :: Update s x c
  -- ^ We have 'NoUpdate' as so that we can use it as as 'mempty', as otherwise
  -- we wouldn't be able to implement it without knowing about @y@.
  AnUpdate :: WrappedUpdate s x c -> Update s x c
  -- ^ We have an extra 'WrappedUpdate' so that in 'withWrappedUpdates' can
  -- avoid dealing with 'NoUpdate'.

instance (Ord x) => Eq (Update s x c) where
  AnUpdate l == AnUpdate r               = l == r
  AnUpdate (WrappedUpdate l) == NoUpdate = l == mempty
  NoUpdate == AnUpdate (WrappedUpdate r) = mempty == r
  NoUpdate == NoUpdate                   = True
  {-# INLINABLE (==) #-}

instance (Ord x) => Ord (Update s x c) where
  AnUpdate l `compare` AnUpdate r               = l `compare` r
  AnUpdate (WrappedUpdate l) `compare` NoUpdate = l `compare` mempty
  NoUpdate `compare` AnUpdate (WrappedUpdate r) = mempty `compare` r
  NoUpdate `compare` NoUpdate                   = EQ
  {-# INLINABLE compare #-}
  AnUpdate l <= AnUpdate r               = l <= r
  AnUpdate (WrappedUpdate l) <= NoUpdate = l <= mempty
  NoUpdate <= AnUpdate (WrappedUpdate r) = mempty <= r
  NoUpdate <= NoUpdate                   = True
  {-# INLINABLE (<=) #-}

deriving stock instance (Show x, Entails1 c Show) => Show (Update s x c)

instance (Ord x) => Semigroup (Update s x c) where
  AnUpdate l <> AnUpdate r  = AnUpdate (l <> r)
  AnUpdate l <> NoUpdate    = AnUpdate l
  NoUpdate   <> AnUpdate r  = AnUpdate r
  NoUpdate   <> NoUpdate    = NoUpdate
  {-# INLINABLE (<>) #-}

instance (Ord x) => Monoid (Update s x c) where
  mempty = NoUpdate
  {-# INLINE mempty #-}

--------------------------------------------------------------------------------

-- The nominal role for @s@ is so that users can't 'coerce' 'Config's.
type role Config nominal nominal representational
data Config (s :: k) (x :: Type) (c :: Type -> Constraint) where
  Config :: (c y, Ord y) => Proxy y -> Chart.Config s' x -> Config s x c
  -- ^ We capture @y@ and its constraints so that we have them available
  -- for 'initial'.
  --
  -- Notice that since 'Config's are only ever constructed by 'configure',
  -- If we have two 'Config's indexed by the same @s@, then their @y@s and
  -- @s'@ can be assumed to be equal. 'withConfig' uses this knowledge.

instance (Eq (Diff x)) => Eq (Config s x c) where
  l == r = withConfigs l r (const (==))
  {-# INLINE (==) #-}

instance (Ord (Diff x)) => Ord (Config s x c) where
  compare l r = withConfigs l r (const compare)
  {-# INLINE compare #-}
  l <= r = withConfigs l r (const (<=))
  {-# INLINE (<=) #-}

instance (Show (Diff x)) => Show (Config s x c) where
  showsPrec n (Config _ c) = showsPrec n c

-- | See comment in the 'Config' constructor to understand why this is safe.
withConfigs
  :: forall s x c a
  .  Config s x c
  -> Config s x c
  -> (forall k (s' :: k) y
        .  (c y)
        => Proxy y
        -> Chart.Config s' x
        -> Chart.Config s' x
        -> a )
  -> a
withConfigs (Config (py :: Proxy yl) (l :: Chart.Config (sl :: slk) x))
            (Config (_  :: Proxy yt) (r :: Chart.Config (sr :: srk) x)) f
  | HRefl <- unsafeCoerce HRefl :: sl :~~: sr
  , Refl  <- unsafeCoerce Refl  :: yl :~:  yr
  = f py l r

--------------------------------------------------------------------------------

configure
  :: forall s' x y c a
  .  (c y, Ord y)
  => Chart.Config s' x
  -> (forall s
        .  Config s x c
        -> (Chart.Update s' x y -> Update s x c)
        -> a )
  -> a
configure c f = f (Config (Proxy @y) c) (AnUpdate . WrappedUpdate)

-- | Apply an 'Update' to the 'XChart' to obtain a new 'XChart'.
-- The laws are the same as those in @"Chartfold.Chart".'Chart.update'@.

-- This unsafeCoercing is safe for reasons similar to those in 'Chart.update'.
-- If the @s@ in 'Update' and 'XChart' agree, then @su ~~ sa@ and @yu ~ ya@.
-- This is because, in practice, the @s@ is always an uninstantiated type
-- variable picked by 'configure', and there are no means other than
-- 'configure' and 'mempty' to construct 'Update's or 'XChart's. In the
-- 'mempty' case, we avoid any coercing and work.
update :: forall s x c. (Ord x) => Update s x c -> XChart s x c -> XChart s x c
update NoUpdate z = z
update (AnUpdate (WrappedUpdate (u :: Chart.Update (su :: suk) x yu)))
       (XChart                  (a :: Chart        (sa :: sak) x ya))
  | HRefl <- unsafeCoerce HRefl :: su :~~: sa
  , Refl  <- unsafeCoerce Refl  :: yu :~:  ya
  = MkXChart (Chart.update u a)

