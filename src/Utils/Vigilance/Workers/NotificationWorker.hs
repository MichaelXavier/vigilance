module Utils.Vigilance.Workers.NotificationWorker (runWorker) where

import Control.Monad (sequence)
import Control.Monad.Trans (lift)
import Control.Lens
import Data.Acid (AcidState)

import Utils.Vigilance.TableOps
import Utils.Vigilance.Types

sendNotifications :: [EWatch] -> [Notifier] -> LogCtxT IO ()
sendNotifications ws = sequence_ . map ($ ws)

--TODO: sendNotifications should probably return a list of successfully sent watches so we can mark those notified
runWorker :: AcidState AppState -> [Notifier] -> LogCtxT IO ()
runWorker acid notifiers = do watches <- getNotifyingS acid
                              sendNotifications watches notifiers
                              completeNotifyingS acid $ map (view watchId) watches
