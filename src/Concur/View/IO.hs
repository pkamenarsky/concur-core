{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Concur.View.IO where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad (forever)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import Data.IORef

import Debug.Trace


type Path = [Int]
type Frame = Int

data Patch v = Patch Int [(Path, v)] | Blocked Int
  deriving Show

patchIndex :: Patch v -> Int
patchIndex (Patch i _) = i
patchIndex (Blocked i) = i

data Ctx v = Ctx
  { ctxIndex :: Int
  , ctxPatch :: MVar (Patch v)
  }

-- NOTE: don't use forkIO in Views - the View thread may be killed at anytime, leaving
-- orphaned threads hanging around

newtype View v a = View (ReaderT (Ctx v) IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

run :: Show v => View v a -> IO a
run (View r) = do
  chPatch <- newEmptyMVar
  bracket (fork chPatch) kill $ \_ -> runReaderT r (Ctx 0 chPatch)
  where
    fork chPatch = forkIO $ forever $ do
      p <- takeMVar chPatch
      traceIO (show p)

    kill = uninterruptibleMask_ . killThread

liftBlockingIO :: IO a -> View v a
liftBlockingIO m = View $ do
  Ctx {..} <- ask
  liftIO $ do
    putMVar ctxPatch (Blocked ctxIndex)
    m

instance (Show v, Monoid v) => Alternative (View v) where
  empty = View $ do
    ctx <- ask

    liftIO $ do
      putMVar (ctxPatch ctx) $ Blocked (ctxIndex ctx)
      threadDelay maxBound

    pure undefined

  View v1 <|> View v2 = View $ do
    ctx <- ask

    liftIO $ do
      res     <- newEmptyMVar
      chPatch <- newEmptyMVar

      bracket (fork res ctx chPatch) kill $ \_ -> takeMVar res

    where
      -- blocked/blocked -> blocked, then before next view -> clear own path
      -- view/blocked, blocked/view, view/view -> clear own path, send assembled view patch

      mkPath ctx paths = [ (ctxIndex ctx:path, v) | (path, v) <- paths ]

      pipe f ctx chPatch = do
        p <- takeMVar chPatch

        case p of
          -- Blocked _     -> putMVar (ctxPatch ctx) $ Blocked (ctxIndex ctx)
          Patch _ paths -> putMVar (ctxPatch ctx) $ Patch (ctxIndex ctx) (f $ mkPath ctx paths)

        pipe id ctx chPatch

      patcher ctx chPatch (Just (Blocked _)) (Just (Blocked _)) = do
        -- traceIO "blocked/blocked"

        putMVar (ctxPatch ctx) $ Blocked (ctxIndex ctx)
        pipe (([ctxIndex ctx], mempty):) ctx chPatch

      patcher ctx chPatch (Just p1) (Just p2) = do
        -- traceIO ("just/just: " <> show p1 <> ", " <> show p2)

        putMVar (ctxPatch ctx) $ Patch (ctxIndex ctx) $ mconcat
          [ [([ctxIndex ctx], mempty)]    -- clear path

          , case p1 of
              Patch _ paths -> mkPath ctx paths
              Blocked _     -> []

          , case p2 of
              Patch _ paths -> mkPath ctx paths
              Blocked _     -> []
          ]

        pipe id ctx chPatch

      patcher ctx chPatch v1 v2 = do
        p <- takeMVar chPatch

        -- traceIO (show p)

        case patchIndex p of
          0 -> patcher ctx chPatch (Just p) v2
          1 -> patcher ctx chPatch v1 (Just p)

      fork res ctx chPatch = do
        tid1 <- forkIO $ runReaderT v1 (Ctx 0 chPatch) >>= putMVar res
        tid2 <- forkIO $ runReaderT v2 (Ctx 1 chPatch) >>= putMVar res
        ptid <- forkIO $ patcher ctx chPatch Nothing Nothing

        pure (tid1, tid2, ptid)

      kill (tid1, tid2, ptid) = uninterruptibleMask_ $ do
        killThread tid1
        killThread tid2
        killThread ptid

        traceIO "killed 3 threads"

view :: v -> View v ()
view v = View $ do
  Ctx {..} <- ask
  liftIO $ putMVar ctxPatch (Patch ctxIndex [([ctxIndex], v)])

--------------------------------------------------------------------------------

v1 :: String -> Int -> View String ()
v1 label wait = go 0
  where
    go n = do
      view label

      liftIO $ do
        traceIO (label <> ": " <> show n)
        threadDelay wait

      go (n + 1)

testviews :: IO ()
testviews = do
  tid <- myThreadId
  run $ do
    -- see if empty fucks up things

    -- empty <|> empty <|> v1 "A" 1000000 <|> v1 "B" 2000000 <|> (v1 "C" 500000 <|> v1 "D" 700000) <|> killMe tid
    v1 "A" 1000000 <|> v1 "B" 2000000 <|> (v1 "C" 500000 <|> v1 "D" 700000) <|> killMe tid
  where
    killMe tid = liftIO $ do
      threadDelay 10000000
      traceIO "killing"
      killThread tid

--------------------------------------------------------------------------------
