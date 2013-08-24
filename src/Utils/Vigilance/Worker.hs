{-# LANGUAGE NoImplicitPrelude #-}
module Utils.Vigilance.Worker ( workForeverWith
                              , workForeverWithDelayed
                              , workForever) where

import ClassyPrelude
import Control.Concurrent (threadDelay)

workForeverWithDelayed :: Int -> (SomeException -> IO ()) -> IO () -> IO ()
workForeverWithDelayed d handler action = workForeverWith handler doAction
  where doAction     = action >> threadDelay microseconds
        microseconds = d * 1000000

workForeverWith :: (SomeException -> IO ()) -> IO () -> IO ()
workForeverWith handler = forever . handleAny handler

-- eats errors
workForever :: IO () -> IO ()
workForever = workForeverWith (const $ return ())
