{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Concur.View.STM where

import Control.Applicative
import Control.Concurrent.STM

import Data.Bifunctor

import qualified Concur.View as V

newtype View v a = View { getView :: V.View (Maybe v) IO STM a }
  deriving (Functor, Applicative, Monad, Alternative)

runView :: (v -> IO ()) -> View v a -> IO a
runView f = V.runView atomically f' . getView
  where
    f' (Just v) = f v
    f' Nothing  = pure ()

mapView :: (u -> v) -> View u a -> View v a
mapView f = View . V.mapView (fmap f) . getView

view :: v -> View v a
view v = View $ V.view (Just v)

orr' :: Semigroup v => [View v a] -> View v (a, View v a)
orr' = fmap (second View) . View . V.orr' . fmap getView

orr :: Semigroup v => [View v a] -> View v a
orr = View . V.orr . fmap getView

andd :: Semigroup v => Monoid a => [View v a] -> View v a
andd = View . V.andd . fmap getView

async :: Monoid v => IO a -> View v a
async f = View $ V.View mempty $ do
  ref <- newEmptyTMVar
  pure
    ( f >>= atomically . putTMVar ref
    , Left $ V.View mempty $ do
        a <- takeTMVar ref
        pure (pure (), Right a)
    )

liftIO :: IO () -> View v ()
liftIO f = View $ V.View Nothing $ pure (f, Right ())
