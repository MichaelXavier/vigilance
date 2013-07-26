module Utils.Vigilance.Sweeper ( expired
                               , sweepWatch
                               , expireWatch) where

import Control.Lens

import Data.Time.Clock.POSIX (POSIXTime)

import Utils.Vigilance.Types


expired :: POSIXTime -> EWatch -> Bool
expired now w@Watch { _watchWState = Active last } = beyondCutoff
  where cutoff       = last + interval
        beyondCutoff = now > cutoff
        interval     = fromInteger $ secondsInterval (w ^. watchInterval)
expired _ _                                        = False

expireWatch :: EWatch -> EWatch
expireWatch w = w & watchWState %~ bumpState
  where bumpState Triggered = Triggered
        bumpState _         = Notifying

sweepWatch :: POSIXTime -> EWatch -> EWatch
sweepWatch t w
  | expired t w = expireWatch w
  | otherwise   = w

secondsInterval :: WatchInterval -> Integer
secondsInterval (Every n Seconds) = n
secondsInterval (Every n Minutes) = n * 60
secondsInterval (Every n Hours)   = n * 3600
secondsInterval (Every n Days)    = n * 86400
secondsInterval (Every n Weeks)   = n * 604800
secondsInterval (Every n Years)   = n * 31536000
