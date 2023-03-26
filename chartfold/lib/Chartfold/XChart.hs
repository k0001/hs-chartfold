{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Chartfold.XChart {--}
 ( XChart(..)
 , update
 , Update(..)
 , initial
 , Err(..)
 ) --}
 where

import Data.AffineSpace (AffineSpace(..))
import Data.Constraint
import Data.Kind
import Data.Sequence (Seq)
import Data.Type.Equality (testEquality)
import GHC.Show (appPrec, appPrec1)
import Type.Reflection qualified as TR

import Chartfold.Chart qualified as Chart
import Chartfold.Constraint (Entails(..), Entails1)

--------------------------------------------------------------------------------

-- | A 'Chart.Chart' whose @y@ has been existentialized.
data XChart (x :: Type) (c :: Type -> Constraint) where
  XChart :: c y => Chart.Chart x y -> XChart x c

withXChart
  :: XChart x c
  -> (forall y. c y => Chart.Chart x y -> r)
  -> r -- ^
withXChart (XChart c) f = f c

instance
  ( Show (Diff x)
  , Show x
  , Entails1 c Show
  ) => Show (XChart x c) where
  showsPrec n x =
    withXChart x $ \(c :: Chart.Chart x y) ->
    withDict (entails :: c y :- Show y) $
      showParen (n > 10) $
        showString "XChart " .
        showsPrec 11 c

instance
  ( Eq (Diff x)
  , Eq x
  , Entails1 c Eq
  , Entails1 c Typeable
  ) => Eq (XChart x c) where
  a == b =
    withXChart a $ \(ca :: Chart.Chart x ya) ->
    withXChart b $ \(cb :: Chart.Chart x yb) ->
    withDict (entails :: c ya :- Typeable ya) $
    withDict (entails :: c yb :- Typeable yb) $
      let tya = TR.typeRep :: TR.TypeRep ya
          tyb = TR.typeRep :: TR.TypeRep yb
      in case testEquality tya tyb of
           Just Refl -> withDict (entails :: c ya :- Eq ya) $ ca == cb
           Nothing   -> False

instance
  ( Show (Diff x)
  , Show x
  , Entails1 c Show
  ) => Show (Update x c) where
  showsPrec n (Update (u :: Chart.Update x y)) =
    withDict (entails :: c y :- Show y) $
    showsPrec n u

instance
  ( Eq (Diff x)
  , Eq x
  , Entails1 c Eq
  , Entails1 c Typeable
  ) => Eq (Update x c) where
  (==) (Update (ua :: Chart.Update x ya))
       (Update (ub :: Chart.Update x yb)) =
          withDict (entails :: c ya :- Typeable ya) $
          withDict (entails :: c yb :- Typeable yb) $
          case testEquality (TR.typeRep @ya) (TR.typeRep @yb) of
            Just Refl -> withDict (entails :: c ya :- Eq ya) (ua == ub)
            Nothing   -> False

deriving anyclass instance
  ( Typeable (Err x c)
  , Show (Err x c)
  ) => Exception (Err x c)

instance (Show x, Entails1 c Show) => Show (Err x c) where
  showsPrec n = showParen (n > appPrec) . \case
    Err_UnknownY -> showString "Err_UnknownY"
    Err_Chart (e :: Chart.Err x y) ->
      withDict (entails :: c y :- Show y) $
      showParen (n > appPrec) $
        showString "Err_Chart " .
        showsPrec appPrec1 e

instance
  ( Eq x
  , Entails1 c Eq
  , Entails1 c Typeable
  ) => Eq (Err x c) where
  (==) (Err_Chart (ea :: Chart.Err x ya))
       (Err_Chart (eb :: Chart.Err x yb)) =
          withDict (entails :: c ya :- Typeable ya) $
          withDict (entails :: c yb :- Typeable yb) $
          case testEquality (TR.typeRep @ya) (TR.typeRep @yb) of
            Just Refl -> withDict (entails :: c ya :- Eq ya) (ea == eb)
            Nothing   -> False
  (==) Err_UnknownY Err_UnknownY = True
  (==) _ _ = False

data Update (x :: Type) (c :: Type -> Constraint) where
  Update :: c y => Chart.Update x y -> Update x c

data Err (x :: Type) (c :: Type -> Constraint) where
  Err_UnknownY :: Err x c
  Err_Chart :: c y => Chart.Err x y -> Err x c

initial :: forall x y c. (Ord x, c y) => Proxy y -> Chart.Config x -> XChart x c
initial _ = XChart . Chart.initial @x @y

update
  :: forall x c
  .  ( AffineSpace x
     , Ord x
     , Entails1 c Ord
     , Entails1 c Typeable )
  => x
  -> Seq (Update x c)
  -> XChart x c
  -> Either (Err x c) (XChart x c)
update x xus (XChart (s :: Chart.Chart x yc)) =
  withDict (entails :: c yc :- Ord yc) $
  withDict (entails :: c yc :- Typeable yc) $ do
    us <- for xus $ \(Update (u :: Chart.Update x yu)) ->
      withDict (entails :: c yu :- Typeable yu) $
      case testEquality (TR.typeRep @yc) (TR.typeRep @yu) of
        Just Refl -> Right u
        Nothing -> Left Err_UnknownY
    bimap Err_Chart XChart $ Chart.update x us s

