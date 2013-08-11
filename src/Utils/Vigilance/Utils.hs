module Utils.Vigilance.Utils ( watchIntervalSeconds
                             , WakeSig
                             , newWakeSig
                             , wakeUp
                             , waitForWake
                             , bindM3
                             , bindM2) where

import Control.Monad ( join
                     , liftM3
                     , liftM2 )
import Data.Time.Clock ( UTCTime
                       , addUTCTime)

import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TMVar ( TMVar
                                    , newEmptyTMVarIO
                                    , takeTMVar
                                    , putTMVar)

import Utils.Vigilance.Types

watchIntervalSeconds :: WatchInterval -> Integer
watchIntervalSeconds (Every n Seconds) = n
watchIntervalSeconds (Every n Minutes) = n * 60
watchIntervalSeconds (Every n Hours)   = n * 60 * 60
watchIntervalSeconds (Every n Days)    = n * 60 * 60 * 24
watchIntervalSeconds (Every n Weeks)   = n * 60 * 60 * 24 * 7
watchIntervalSeconds (Every n Years)   = n * 60 * 60 * 24 * 365

stepClock :: WatchInterval -> UTCTime -> UTCTime
stepClock int = addUTCTime $ fromInteger $ watchIntervalSeconds int

bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f m1 m2  = join $ liftM2 f m1 m2

bindM3 :: Monad m => (a -> b -> c -> m d) -> m a -> m b -> m c -> m d
bindM3 f m1 m2 m3  = join $ liftM3 f m1 m2 m3

type WakeSig a = TMVar a

newWakeSig :: IO (WakeSig a)
newWakeSig = newEmptyTMVarIO

waitForWake :: WakeSig a -> IO a
waitForWake = atomically . takeTMVar

wakeUp :: WakeSig a -> a -> IO ()
wakeUp sig = atomically . putTMVar sig
