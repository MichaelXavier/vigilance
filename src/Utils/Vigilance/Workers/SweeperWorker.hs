module Utils.Vigilance.Workers.SweeperWorker (runWorker) where

import Data.Acid (AcidState)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Utils.Vigilance.TableOps
import Utils.Vigilance.Types

runWorker :: AcidState AppState -> IO ()
runWorker acid = sweepTableS acid =<< getPOSIXTime
