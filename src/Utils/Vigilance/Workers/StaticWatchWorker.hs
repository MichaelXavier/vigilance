{-# LANGUAGE OverloadedStrings #-}
module Utils.Vigilance.Workers.StaticWatchWorker (runWorker) where

import Data.Acid (AcidState)
import Control.Applicative ((<$>))
import Control.Concurrent.STM ( TChan
                              , atomically
                              , readTChan )
import Control.Monad.Trans (lift)
import Control.Lens
import Data.Text (Text)
import Utils.Vigilance.Config ( reloadConfig
                              , convertConfig )
import Utils.Vigilance.Logger ( pushLog
                              , renameLogCtx )
import Utils.Vigilance.TableOps (mergeStaticWatchesS)
import Utils.Vigilance.Types
import Utils.Vigilance.Utils ( WakeSig
                             , waitForWake )

runWorker :: AcidState AppState -> TChan Config -> LogCtxT IO ()
runWorker acid cfgChan = renameLogCtx "Watch Config Monitor" $ do
  pushLog "Waiting for new watches"
  cfg <- lift $ atomically $ readTChan cfgChan
  pushLog "Merging new static watches"
  lift $ mergeStaticWatchesS acid $ cfg ^. configWatches
  pushLog "Static watches merged"
