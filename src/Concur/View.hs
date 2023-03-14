{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Concur.View where

import Control.Monad.Trans.Class
import Control.Applicative

import Data.Bifunctor


data View v m stm a = View v (stm (m (), Either (View v m stm a) a))

step :: Monad stm => Alternative alt => View (alt v) m stm a -> stm (m (), Either (View (alt v) m stm a) a)
step (View v n) = do
  (io, next) <- n
  case next of
    Left (View v' n') -> pure (io, Left (View (v' <|> v) n'))
    Right a           -> pure (io, Right a)

runView :: Monad m => Monad stm => Alternative alt => (forall b. stm b -> m b) -> (alt v -> m ()) -> View (alt v) m stm a -> m a
runView embed f rview@(View v _) = do
  f v
  (io, next) <- embed (step rview)
  io
  case next of
    Left rview' -> runView embed f rview'
    Right a     -> pure a

mapViewWith :: Functor stm => (u -> u -> u) -> (u -> v) -> View u m stm a -> View v m stm a
mapViewWith alt f rview@(View rv _) = preserve rv rview
  where
    preserve v (View v' n) = View (f v') (fmap (fmap (first (preserve (v' `alt` v)))) n)

mapView :: Functor stm => Alternative alt => (alt u -> v) -> View (alt u) m stm a -> View v m stm a
mapView = mapViewWith (<|>)

instance Functor stm => Functor (View v m stm) where
  fmap f (View v n)  = View v (fmap (fmap (bimap (fmap f) f)) n)

instance (Applicative m, Monad stm, Alternative stm, Semigroup v) => Monad (View (Maybe v) m stm) where
  rview@(View v _) >>= m = View v $ do
    (io, r) <- step rview
    case r of
      Left view' -> pure (io, Left (view' >>= m))
      Right a    -> pure (io, Left (m a))

instance (Applicative m, Monad stm, Alternative stm, Semigroup v) => Applicative (View (Maybe v) m stm) where
  pure a = View Nothing (pure (pure (), Right a))
  u1 <*> u2 = mapViewWith alt (uncurry (<>)) (go u1 u2)
    where
      alt (Nothing, Nothing) b = b
      alt a _ = a

      go view1@(View v1 _) view2@(View v2 _) = View (v1, v2) $ do
        r <- fmap Left (step view1) <|> fmap Right (step view2)
        case r of
          Left (io, Left view1')  -> pure (io, Left (go view1' view2))
          Left (io, Right a)      -> pure (io, Left (a <$> mapView (v1,) view2))
          Right (io, Left view2') -> pure (io, Left (go view1 view2'))
          Right (io, Right a)     -> pure (io, Left (($ a) <$> mapView (,v2) view1))

instance (Applicative m, Monad stm, Alternative stm, Semigroup v) => Alternative (View (Maybe v) m stm) where
  empty = view Nothing
  (<|>) = vor

instance (Applicative m, Monoid v) => MonadTrans (View v m) where
  lift f = View mempty $ do
    a <- f
    pure (pure (), Right a)

view :: Alternative stm => v -> View v m stm a
view v = View v empty

vor' :: Monad stm => Alternative stm => Semigroup v
  => View (Maybe v) m stm a
  -> View (Maybe v) m stm b
  -> View (Maybe v) m stm (Either (a, View (Maybe v) m stm b) (b, View (Maybe v) m stm a))
vor' u1 u2 = mapViewWith alt (uncurry (<>)) (go u1 u2)
  where
    alt (Nothing, Nothing) b = b
    alt a _ = a

    go view1@(View v1 _) view2@(View v2 _) = View (v1, v2) $ do
      r <- fmap Left (step view1) <|> fmap Right (step view2)
      case r of
        Left (io, Left view1')  -> pure (io, Left (go view1' view2))
        Left (io, Right a)      -> pure (io, Right (Left (a, view2)))
        Right (io, Left view2') -> pure (io, Left (go view1 view2'))
        Right (io, Right a)     -> pure (io, Right (Right (a, view1)))

vor :: Monad stm => Alternative stm => Semigroup v => View (Maybe v) m stm a -> View (Maybe v) m stm a -> View (Maybe v) m stm a
vor u1 u2 = fromEither <$> vor' u1 u2
  where
    fromEither (Left (a, _))  = a
    fromEither (Right (a, _)) = a

orr' :: Monad stm => Alternative stm => Semigroup v => Applicative m => [View (Maybe v) m stm a] -> View (Maybe v) m stm (a, View (Maybe v) m stm a)
orr' = foldr f (view Nothing)
  where
    f :: Monad stm => Alternative stm => Applicative m => Semigroup v
      => View (Maybe v) m stm a
      -> View (Maybe v) m stm (a, View (Maybe v) m stm a)
      -> View (Maybe v) m stm (a, View (Maybe v) m stm a)
    f u1 u2 = do
      r <- u1 `vor'` u2
      case r of
        Left (a, u2')         -> pure (a, do
                                          (a', n) <- u2'
                                          pure a' `vor` n
                                      )
        Right ((a, u2'), u1') -> pure (a, u1' `vor` u2')

orr :: Monad stm => Alternative stm => Semigroup v => [View (Maybe v) m stm a] -> View (Maybe v) m stm a
orr = foldr vor (view Nothing)

vand :: Applicative m => Monad stm => Alternative stm => Semigroup v => Semigroup a => View (Maybe v) m stm a -> View (Maybe v) m stm a -> View (Maybe v) m stm a
vand u1 u2 = fmap (<>) u1 <*> u2

andd :: Applicative m => Monad stm => Alternative stm => Semigroup v => Monoid a => [View (Maybe v) m stm a] -> View (Maybe v) m stm a
andd = foldr vand (pure mempty)

liftAction :: Functor m => Applicative stm => Monoid v => m () -> View v m stm ()
liftAction f = View mempty $ pure (f, Right ())
