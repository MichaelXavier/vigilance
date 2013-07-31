module Utils.Vigilance.Workers.SweeperWorker (runWorker) where

import Data.Time.Clock.POSIX (getPOSIXTime)

import Utils.Vigilance.TableOps
import Utils.Vigilance.Types

runWorker acid = sweepTableS acid =<< getPOSIXTime
