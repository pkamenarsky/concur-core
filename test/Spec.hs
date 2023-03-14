module Main where

import Control.Monad.Trans.Class

import Data.Functor.Identity
import Data.Maybe (fromMaybe)
import Data.List (group)

import Concur.View

import Debug.Trace

import System.Exit

--------------------------------------------------------------------------------

type TView = View (Maybe String) Identity Maybe

showView :: Show a => TView a -> String
showView rview@(View v _) = fromMaybe "" v <> " >> " <> showNext (step rview)
  where
    showNext Nothing = "BLOCK"
    showNext (Just (_, Left n)) = showView n
    showNext (Just (_, Right a)) = show a

cmp :: Eq a => TView a -> TView a -> Bool
cmp view1@(View v1 _) view2@(View v2 _) = v1 == v2 && cmpr r1 r2
  where
    r1 = step view1
    r2 = step view2

    cmpr Nothing Nothing = True
    cmpr (Just (io1, Left view1')) (Just (io2, Left view2')) = io1 == io2 && cmp view1' view2'
    cmpr (Just (io1, Right a1)) (Just (io2, Right a2)) = io1 == io2 && a1 == a2
    cmpr _ _ = False

viewToList :: Show a => TView a -> [Maybe String]
viewToList = fmap head . group . go
  where
    go rview@(View v _) = v:showNext (step rview)
      where
        showNext Nothing = [ Just "BLOCKED" ]
        showNext (Just (_, Left n)) = go n
        showNext (Just (_, Right a)) = [ Just (show a) ]

cmpNubbed :: Show a => TView a -> TView a -> Bool
cmpNubbed v1 v2
  | viewToList v1 == viewToList v2 = True
  | otherwise = trace (show (viewToList v1) <> " should be " <> show (viewToList v2)) False

toViews :: [Maybe String] -> a -> TView a
toViews []     r = View Nothing (Just (Identity (), Right r))
toViews [a] r    = View a (Just (Identity (), Right r))
toViews (a:as) r = View a (Just (Identity (), Left (toViews as r)))

t1 :: Bool
t1 = cmpNubbed is should
  where
    is = do
      _ <- orr [ view (Just "a"), lift (Just "") ] 
      pure "J"

    should = toViews [Just "a"] "J"

t2 :: Bool
t2 = cmpNubbed is should
  where

  is = do
    _ <- pure ()
    _ <- orr [ view (Just "a"), lift (Just ()) ] 
    pure "J"
  
  should = toViews [Nothing, Just "a"] "J"

t3 :: Bool
t3 = cmpNubbed is should
  where
    is = do
      _ <- pure ()
      _ <- orr [ view (Just "a"), view (Just "b"), lift (Just ()) ] 
      _ <- pure ()
      pure "J"
  
    should = toViews [Nothing, Just "ab"] "J"

t4 :: Bool
t4 = cmpNubbed is should
  where
    is = do
      _ <- orr [ view (Just "a"), view (Just "b"), view (Just "c"), lift (Just ()) ]
      _ <- orr
        [ do
            _ <- pure ()
            _ <- pure ()
            _ <- view (Just "c")
            _ <- pure ()
            pure ()
        , do
            _ <- pure ()
            _ <- pure ()
            _ <- pure ()
            _ <- pure ()
            lift (Just ())
            _ <- pure ()
            pure ()
        ]
      pure "J"

    should = toViews [Just "abc", Just "c"] "J"

t5 :: Bool
t5 = cmpNubbed is should
  where
    is = do
      _ <- orr [ view (Just "a"), view (Just "b"), view (Just "c"), lift (Just ()) ]
      _ <- orr
        [ do
            _ <- pure ()
            _ <- pure ()
            _ <- view (Just "c")
            _ <- pure ()
            pure ()
        , do
            _ <- pure ()
            _ <- pure ()
            _ <- pure ()
            _ <- pure ()
            orr [ view (Just "d"), lift (Just ()) ]
            _ <- pure ()
            pure ()
        ]
      pure "J"

    should = toViews [Just "abc", Just "c", Just "cd"] "J"

t6 :: Bool
t6 = cmpNubbed is should
  where
    is :: TView String
    is = andd [ andd [ andd [ pure "J" , pure "H" ] ] ]

    should = toViews [Nothing] "JH"

t7 :: Bool
t7 = cmpNubbed is should
  where
    is :: TView String
    is = andd
      [ do
          _ <- pure ()
          _ <- pure ()
          _ <- orr [ view (Just "c"), lift (Just ()) ]
          _ <- pure ()
          pure "J"
      , do
          _ <- pure ()
          _ <- pure ()
          _ <- pure ()
          _ <- pure ()
          _ <- orr [ view (Just "d"), lift (Just ()) ]
          _ <- pure ()
          pure "H"
      ]

    should = toViews [Nothing, Just "c", Just "cd"] "JH"

t8 :: Bool
t8 = cmpNubbed (andd [ is, is, is ]) should
  where
    is :: TView String
    is = andd
      [ do
          _ <- pure ()
          _ <- pure ()
          _ <- orr [ view (Just "c"), lift (Just ()) ]
          _ <- pure ()
          pure "J"
      , do
          _ <- pure ()
          _ <- pure ()
          _ <- pure ()
          _ <- pure ()
          _ <- orr [ view (Just "d"), lift (Just ()) ]
          _ <- pure ()
          pure "H"
      ]

    should = toViews [Nothing, Just "c", Just "cd", Just "cdc", Just "cdcd", Just "cdcdc", Just "cdcdcd"] "JHJHJH"

t9 :: Bool
t9 = cmpNubbed (andd [ is, is, is ] >> andd [ is, is, is ]) should
  where
    is :: TView String
    is = andd
      [ do
          _ <- pure ()
          _ <- pure ()
          _ <- orr [ lift (Just ()), view (Just "c") ]
          _ <- pure ()
          pure "J"
      , do
          _ <- pure ()
          _ <- pure ()
          _ <- pure ()
          _ <- pure ()
          _ <- orr [ lift (Just ()), view (Just "d") ]
          _ <- pure ()
          _ <- pure ()
          _ <- pure ()
          pure "H"
      ]

    should = toViews
      [Nothing, Just "c", Just "cd", Just "cdc", Just "cdcd", Just "cdcdc", Just "cdcdcd", Just "c", Just "cd", Just "cdc", Just "cdcd", Just "cdcdc", Just "cdcdcd"] "JHJHJH"

t10 :: Bool
t10 = cmpNubbed is should
  where
    is = do
      (_, n) <- orr'
        [ pure ()
        , do
            orr [ pure (), view (Just "a") ]
            orr [ pure (), view (Just "b") ]
            orr [ pure (), view (Just "c") ] 
        ]
      n
      pure "J"
  
    should = toViews [Just "a", Just "b", Just "c"] "J"

--------------------------------------------------------------------------------

main :: IO ()
main = if r
  then putStrLn "Passed"
  else do
    putStrLn "Failed"
    exitFailure
  where
    r = and [ t1, t2, t3, t4, t5, t6, t7, t8, t9, t10 ]
