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

type Path = [Int]
type Frame = Int

data Ctx v = Ctx
  { ctxPath  :: [Int]
  , ctxPatch :: Frame -> Path -> v -> IO ()
  , ctxFrame :: IORef Frame
  }

newtype View v a = View (ReaderT (Ctx v) IO a)
  deriving (Functor, Applicative, Monad)

-- TODO: MonadIO; the problem is that spawned threads in Views will not be killed
-- automatically with an unrestricted MonadIO instance

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
        t2 <- forkIO $ runReaderT v1 (mkCtx f2 (1:) ctx) >>= putMVar res

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
  frame <- liftIO $ atomicModifyIORef' ctxFrame $ \f -> (f + 1, f)
  liftIO $ ctxPatch frame ctxPath v
