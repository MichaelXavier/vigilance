{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Utils.Vigilance.Workers.NotificationWorker ( runWorker
                                                  , sendNotificationsWithRetry) where

import ClassyPrelude
import Control.Lens
import Data.Acid (AcidState)

import Utils.Vigilance.Logger
import Utils.Vigilance.TableOps
import Utils.Vigilance.Types
import Utils.Vigilance.Utils (concatMapM)

sendNotifications :: [EWatch] -> NotifierGroup -> LogCtxT IO [FailedNotification]
sendNotifications ws ns = concatMapM ($ ws) $ extractNotifiers ns

sendNotificationsWithRetry :: AcidState AppState -> [EWatch] -> NotifierGroup -> LogCtxT IO ()
sendNotificationsWithRetry acid watches notifiers = do
  failures <- sendNotifications watches notifiers
  addFailedNotificationsS acid failures

runWorker :: AcidState AppState -> NotifierGroup -> LogCtxT IO ()
runWorker acid notifiers = renameLogCtx "Notifier Worker" $ do
                              watches <- getNotifyingS acid
                              sendNotificationsWithRetry acid watches notifiers
                              unless (null watches) $ pushLog $ notifyingMsg watches
                              completeNotifyingS acid $ map (view watchName) watches

notifyingMsg :: [EWatch] -> Text
notifyingMsg watches = mconcat ["Notifying for ", length' watches, " watches: ", names]
  where length' = show . length
        names   = intercalate ", " $ map (view (watchName . unWatchName)) watches

extractNotifiers :: NotifierGroup -> [Notifier]
extractNotifiers NotifierGroup {..} = catMaybes [ view notifier <$> _ngEmail
                                                , Just $ _ngLog ^. notifier
                                                , Just $ _ngHTTP ^. notifier]
