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

type Patch v = [(Path, v)]

data Ctx v = Ctx
  { ctxPath  :: [Int]
  , ctxPatch :: MVar (Patch v)
  }

-- NOTE: don't use forkIO in Views - the View thread may be killed at anytime, leaving
-- orphaned threads hanging around

newtype View v a = View (ReaderT (Ctx v) IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

run :: Show v => View v a -> IO a
run (View r) = do
  ctxPatch <- newEmptyMVar
  runReaderT r (Ctx [] ctxPatch)

instance Alternative (View v) where
  empty = View $ liftIO $ myThreadId >>= killThread >> pure undefined

  View v1 <|> View v2 = View $ do
    ctx <- ask

    liftIO $ do
      res <- newEmptyMVar
      bracket (fork ctx res) kill $ \_ -> takeMVar res

    where
      patch = do
        undefined

      fork ctx res = do
        tid1 <- forkIO $ runReaderT v1 (mkCtx (0:) ctx) >>= putMVar res
        tid2 <- forkIO $ runReaderT v2 (mkCtx (1:) ctx) >>= putMVar res
        ptid <- forkIO patch

        pure (tid1, tid2, ptid)

      kill (tid1, tid2, ptid) = uninterruptibleMask_ $ do
        killThread tid1
        killThread tid2
        killThread ptid

      mkCtx pathf ctx = ctx
        { ctxPath  = pathf (ctxPath ctx)
        }

view :: v -> View v ()
view v = View $ do
  Ctx {..} <- ask

  liftIO $ do
    putMVar ctxPatch [(ctxPath, v)]

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
    empty <|> empty <|> v1 "A" 1000000 <|> v1 "B" 2000000 <|> (v1 "C" 500000 <|> v1 "D" 700000) <|> killMe tid
  where
    killMe tid = liftIO $ do
      threadDelay 5000000
      traceIO "killing"
      killThread tid

--------------------------------------------------------------------------------
