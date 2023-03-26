{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Chartfold.XChart
 ( XChart(..)
 , withXChart
 , Config(Config)
 , withConfigXChart
 , Update(Update)
 , withUpdateXChart
 , Err(ErrXChart_UnknownY, ErrXChart_Chart)
 ) where

import Data.AffineSpace (AffineSpace(..))
import Data.Constraint (withDict, (:-))
import Type.Reflection qualified as TR
import Data.Type.Equality (testEquality)

import Chartfold.Core
import Chartfold.Chart qualified as Chart
import Chartfold.Constraint

{-
-- | Overlap two charts having the same @x@ axis, but different @y@ axis.
data XChartLR x cl cr = XChartLR (XChart x cl) (XChart x cr)
-}

-- | A 'Chart.Chart' whose @y@ has been existentialized.
data XChart x c where
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


withUpdateXChart
  :: Update (XChart x c)
  -> (forall y. c y => Update (Chart.Chart x y) -> r)
  -> r -- ^
withUpdateXChart (Update u) f = f u

instance
  ( Show (Diff x)
  , Show x
  , Entails1 c Show
  ) => Show (Update (XChart x c)) where
  showsPrec n x =
    withUpdateXChart x $ \(u :: Update (Chart.Chart x y)) ->
    withDict (entails :: c y :- Show y) $
      showParen (n > 10) $
        showString "Update " .
        showsPrec 11 u

instance
  ( Eq (Diff x)
  , Eq x
  , Entails1 c Eq
  , Entails1 c Typeable
  ) => Eq (Update (XChart x c)) where
  a == b =
    withUpdateXChart a $ \(ua :: Update (Chart.Chart x ya)) ->
    withUpdateXChart b $ \(ub :: Update (Chart.Chart x yb)) ->
    withDict (entails :: c ya :- Typeable ya) $
    withDict (entails :: c yb :- Typeable yb) $
      let tya = TR.typeRep :: TR.TypeRep ya
          tyb = TR.typeRep :: TR.TypeRep yb
      in case testEquality tya tyb of
           Just Refl -> withDict (entails :: c ya :- Eq ya) $ ua == ub
           Nothing   -> False

withConfigXChart
  :: Config (XChart x c)
  -> (forall y. c y => Config (Chart.Chart x y) -> r)
  -> r -- ^
withConfigXChart (Config d) f = f d

instance
  ( Show (Diff x)
  , Show x
  , Entails1 c Show
  ) => Show (Config (XChart x c)) where
  showsPrec n x =
    withConfigXChart x $ \(d :: Config (Chart.Chart x y)) ->
    withDict (entails :: c y :- Show y) $
      showParen (n > 10) $
        showString "Config " .
        showsPrec 11 d

instance
  ( Eq (Diff x)
  , Eq x
  , Entails1 c Eq
  , Entails1 c Typeable
  ) => Eq (Config (XChart x c)) where
  a == b =
    withConfigXChart a $ \(da :: Config (Chart.Chart x ya)) ->
    withConfigXChart b $ \(db :: Config (Chart.Chart x yb)) ->
    withDict (entails :: c ya :- Typeable ya) $
    withDict (entails :: c yb :- Typeable yb) $
      let tya = TR.typeRep :: TR.TypeRep ya
          tyb = TR.typeRep :: TR.TypeRep yb
      in case testEquality tya tyb of
           Just Refl -> withDict (entails :: c ya :- Eq ya) $ da == db
           Nothing   -> False

instance (Typeable (Err (XChart x c)), Show (Err (XChart x c)))
  => Exception (Err (XChart x c))

instance (Show x, Entails1 c Show) => Show (Err (XChart x c)) where
  showsPrec n = showParen (n > 10) . \case
    ErrXChart_UnknownY -> showString "ErrXChart_UnknownY"
    ErrXChart_Chart (e :: Err (Chart.Chart x y)) ->
      withDict (entails :: c y :- Show y) $
        showParen (n > 10) $
          showString "ErrXChart_Chart " .
          showsPrec 11 e

instance
  ( Eq x
  , Entails1 c Eq
  , Entails1 c Typeable
  ) => Eq (Err (XChart x c)) where
  (==) ErrXChart_UnknownY ErrXChart_UnknownY = True
  (==) (ErrXChart_Chart (ea :: Err (Chart.Chart x ya)))
       (ErrXChart_Chart (eb :: Err (Chart.Chart x yb)))
     = withDict (entails :: c ya :- Typeable ya) $
       withDict (entails :: c yb :- Typeable yb) $
         let tya = TR.typeRep :: TR.TypeRep ya
             tyb = TR.typeRep :: TR.TypeRep yb
         in case testEquality tya tyb of
              Just Refl -> withDict (entails :: c ya :- Eq ya) $ ea == eb
              Nothing   -> False
  _ == _ = False


instance
  forall x c.
  ( AffineSpace x
  , Ord x
  , Entails1 c Ord
  , Entails1 c Typeable
  ) => Element x (XChart x c) where

  data instance Update (XChart x c) where
    Update :: c y => Update (Chart.Chart x y) -> Update (XChart x c)

  data instance Config (XChart x c) where
    Config :: c y => Config (Chart.Chart x y) -> Config (XChart x c)

  data instance Err (XChart x c) where
    ErrXChart_UnknownY :: Err (XChart x c)
    ErrXChart_Chart :: c y => Err (Chart.Chart x y) -> Err (XChart x c)

  element xd =
    withConfigXChart xd $ \(d :: Config (Chart.Chart x y)) ->
    withDict (entails :: c y :- Ord y) $
    XChart (element d)

  update x xus xc =
    withXChart xc $ \(c :: Chart.Chart x yc) ->
    withDict (entails :: c yc :- Typeable yc) $
    withDict (entails :: c yc :- Ord yc) $ do
      let tyc = TR.typeRep :: TR.TypeRep yc
      us <- for xus $ \xu ->
        withUpdateXChart xu $ \(u :: Update (Chart.Chart x yu)) ->
        withDict (entails :: c yu :- Typeable yu) $ do
          let tyu = TR.typeRep :: TR.TypeRep yu
          case testEquality tyu tyc of
            Nothing -> Left ErrXChart_UnknownY
            Just Refl -> Right u
      bimap ErrXChart_Chart XChart $ update x us c

