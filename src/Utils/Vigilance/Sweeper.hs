module Utils.Vigilance.Sweeper ( expired
                               , sweepWatch ) where

import Control.Lens

import Data.Time.Clock.POSIX (POSIXTime)

import Utils.Vigilance.Types


expired :: POSIXTime -> Watch i -> Bool
expired now w@Watch { _watchWState = Active lst } = beyondCutoff
  where cutoff       = lst + interval
        beyondCutoff = now > cutoff
        interval     = fromInteger $ secondsInterval (w ^. watchInterval)
expired _ _                                        = False

expireWatch :: Watch i -> Watch i
expireWatch w = w & watchWState %~ bumpState
  where bumpState Triggered = Triggered
        bumpState _         = Notifying

sweepWatch :: Show i => POSIXTime -> Watch i -> Watch i
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
