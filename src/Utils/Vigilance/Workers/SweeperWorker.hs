{-# LANGUAGE OverloadedStrings #-}
module Utils.Vigilance.Workers.SweeperWorker (runWorker) where

import Control.Monad.Trans (lift)
import Data.Acid (AcidState)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Utils.Vigilance.Logger
import Utils.Vigilance.TableOps
import Utils.Vigilance.Types

runWorker :: AcidState AppState -> LogCtxT IO ()
runWorker acid = renameLogCtx "Sweeper Worker" $ do
                   vLog "Sweeping"
                   lift $ sweepTableS acid =<< getPOSIXTime
