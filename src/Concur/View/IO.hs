{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Concur.View.IO where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad.Fix
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import Data.IORef

import Debug.Trace


type Path = [Int]
type Frame = Int

data Ctx v = Ctx
  { ctxPath  :: [Int]
  , ctxPatch :: Frame -> Path -> v -> IO ()
  , ctxFrame :: IORef Frame
  }

-- NOTE: don't use forkIO in Views - the View thread may be killed at anytime, leaving
-- orphaned threads hanging around

newtype View v a = View (ReaderT (Ctx v) IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

run :: Show v => View v a -> IO a
run (View r) = do
  ctxFrame <- newIORef 0
  runReaderT r (Ctx [] (\frame path v -> traceIO (show frame <> ", " <> show path <> ", " <> show v)) ctxFrame)

instance Alternative (View v) where
  empty = View $ liftIO $ threadDelay maxBound >> pure undefined

  View v1 <|> View v2 = View $ do
    ctx <- ask

    liftIO $ do
      res <- newEmptyMVar
      bracket (fork ctx res) kill $ \_ -> takeMVar res

    where
      fork ctx res = do
        f1  <- newIORef 0
        f2  <- newIORef 0

        t1 <- forkIO $ runReaderT v1 (mkCtx f1 (0:) ctx) >>= putMVar res
        t2 <- forkIO $ runReaderT v2 (mkCtx f2 (1:) ctx) >>= putMVar res

        pure (t1, t2)

      kill (t1, t2) = uninterruptibleMask_ $ do
        killThread t1
        killThread t2

      mkCtx fref pathf ctx = ctx
        { ctxPath  = pathf (ctxPath ctx)
        , ctxFrame = fref
        }

view :: v -> View v ()
view v = View $ do
  Ctx {..} <- ask

  liftIO $ do
    frame <- atomicModifyIORef' ctxFrame $ \f -> (f + 1, f)
    ctxPatch frame ctxPath v

--------------------------------------------------------------------------------

v1 :: String -> Int -> View String ()
v1 label wait = liftIO (go 0)
  where
    go n = do
      traceIO (label <> ": " <> show n)
      threadDelay wait
      go (n + 1)

testviews :: IO ()
testviews = do
  tid <- myThreadId
  run $ do
    v1 "A" 1000000 <|> v1 "B" 2000000 <|> (v1 "C" 500000 <|> v1 "D" 700000) <|> killMe tid
  where
    killMe tid = liftIO $ do
      threadDelay 5000000
      traceIO "killing"
      killThread tid
