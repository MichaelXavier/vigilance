{-# LANGUAGE NoImplicitPrelude #-}
module Utils.Vigilance.Worker ( workForeverWith
                              , workForever) where

import ClassyPrelude
import Control.Monad (forever)
import Control.Exception.Base (SomeException)

-- pointfree?
workForeverWith :: (SomeException -> IO ()) -> IO () -> IO ()
workForeverWith handler = forever . handleAny handler

-- eats errors
workForever :: IO () -> IO ()
workForever = workForeverWith (const $ return ())
