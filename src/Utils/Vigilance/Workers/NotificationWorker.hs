module Utils.Vigilance.Workers.NotificationWorker (runWorker) where

import Control.Monad (mapM_)
import Control.Lens

import Utils.Vigilance.TableOps
import Utils.Vigilance.Types

sendNotifications :: [EWatch] -> [Notifier] -> IO ()
sendNotifications ws = mapM_ ($ ws)

--TODO: sendNotifications should probably return a list of successfully sent watches so we can mark those notified
runWorker acid notifiers = do watches <- getNotifyingS acid
                              sendNotifications watches notifiers
                              completeNotifyingS acid $ map (view watchId) watches
