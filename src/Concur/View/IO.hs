module Concur.View.IO where

import Control.Concurrent
import Control.Concurrent.MVar

import Control.Exception
import Control.Monad.Free
import Control.Monad.IO.Class

import Data.Bifunctor
import Data.IORef
import Data.Typeable

newtype Step a = Step { step :: IO (Either a (Step a)) }

run :: Step a -> IO a
run (Step io) = do
  r <- io
  case r of
    Left a     -> pure a
    Right step -> run step

instance Functor Step where
  fmap f (Step io) = Step (fmap (bimap f (fmap f)) io)

instance Applicative Step where
  pure a = Step $ pure (Left a)
  Step f <*> Step a = Step $ do
    f' <- f
    a' <- a
    case (f', a') of
      (Left  g, Left  x) -> pure $ Left (g x)
      (Left  g, Right x) -> pure $ Right $ pure g <*> x
      (Right g, Left  x) -> pure $ Right $ g <*> pure x
      (Right g, Right x) -> pure $ Right $ g <*> x

instance Monad Step where
  Step m >>= f = Step $ do
    r <- m
    case r of
      Left a   -> step (f a)
      Right m' -> step (m' >>= f)

instance MonadIO Step where
  liftIO io = Step (Left <$> io)

data Suspend a = Suspend (MVar (Step a))

instance Show (Suspend a) where
  show _ = "Suspend"

instance Typeable a => Exception (Suspend a)

suspendable :: Typeable a => MVar (a, ThreadId) -> Step a -> IO ThreadId
suspendable res step = do
  cnt <- newIORef step
  tid <- forkIO (go cnt step `catch` handler cnt)

  pure tid
  where
    handler :: IORef (Step a) -> Suspend a -> IO ()
    handler cnt (Suspend mCnt) = readIORef cnt >>= putMVar mCnt

    go cnt step@(Step io) = do
      r <- io
      case r of
        Left a -> do
          tid <- myThreadId
          putMVar res (a, tid)

        Right step' -> do
          writeIORef cnt step'
          go cnt step'
    
orr :: Typeable a => Step a -> Step a -> Step (a, Step a)
orr s1 s2 = Step $ do
  res <- newEmptyMVar

  tid1 <- suspendable res s1
  tid2 <- suspendable res s2

  (a, tid) <- takeMVar res
  
  mCnt <- newEmptyMVar

  if tid == tid1
    then do
      throwTo tid2 (Suspend mCnt)
    else
      throwTo tid1 (Suspend mCnt)

  cnt <- takeMVar mCnt
  pure (Left (a, cnt))

t1 :: Int -> Int -> Step ()
t1 delay n = do
  liftIO $ print n
  liftIO $ threadDelay delay
  t1 delay (n + 1)

test :: IO ()
test = run $ do
  (_, cnt) <- orr (t1 1000000 0) (liftIO $ threadDelay 3000000)
  liftIO $ print "thread2 died, continuing in a bit"
  liftIO $ threadDelay 3000000
  cnt
