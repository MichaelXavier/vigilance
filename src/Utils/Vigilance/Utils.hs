module Utils.Vigilance.Utils (watchIntervalSeconds) where

import Data.Time.Clock ( UTCTime
                       , addUTCTime)

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
