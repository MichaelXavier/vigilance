{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils.Vigilance.Workers.NotificationWorker ( runWorker
                                                  , sendNotificationsWithRetry) where

import ClassyPrelude
import Control.Lens
import Data.Acid (AcidState)

import Utils.Vigilance.Logger
import Utils.Vigilance.TableOps
import Utils.Vigilance.Types

sendNotifications :: [EWatch] -> [Notifier] -> LogCtxT IO [FailedNotification]
sendNotifications ws ns = concat <$> mapM ($ ws) ns

sendNotificationsWithRetry :: AcidState AppState -> [EWatch] -> [Notifier] -> LogCtxT IO ()
sendNotificationsWithRetry acid watches notifiers = do
  failures <- sendNotifications watches notifiers
  addFailedNotificationsS acid failures

runWorker :: AcidState AppState -> [Notifier] -> LogCtxT IO ()
runWorker acid notifiers = renameLogCtx "Notifier Worker" $ do
                              watches <- getNotifyingS acid
                              sendNotificationsWithRetry acid watches notifiers
                              unless (null watches) $ pushLog $ notifyingMsg watches
                              completeNotifyingS acid $ map (view watchName) watches

notifyingMsg :: [EWatch] -> Text
notifyingMsg watches = mconcat ["Notifying for ", length' watches, " watches: ", names]
  where length' = show . length
        names   = intercalate ", " $ map (view (watchName . unWatchName)) watches
