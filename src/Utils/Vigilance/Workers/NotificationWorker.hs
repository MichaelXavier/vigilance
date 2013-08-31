{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils.Vigilance.Workers.NotificationWorker ( runWorker
                                                  , sendNotifications ) where

import ClassyPrelude
import Control.Lens
import Data.Acid (AcidState)

import Utils.Vigilance.Logger
import Utils.Vigilance.TableOps
import Utils.Vigilance.Types

sendNotifications :: [EWatch] -> [Notifier] -> LogCtxT IO ()
sendNotifications ws = mapM_ ($ ws)

runWorker :: AcidState AppState -> [Notifier] -> LogCtxT IO ()
runWorker acid notifiers = renameLogCtx "Notifier Worker" $ do
                              watches <- getNotifyingS acid
                              unless (null watches) $ pushLog $ notifyingMsg watches
                              sendNotifications watches notifiers
                              completeNotifyingS acid $ map (view watchName) watches

notifyingMsg :: [EWatch] -> Text
notifyingMsg watches = mconcat ["Notifying for ", length' watches, " watches: ", names]
  where length' = show . length
        names   = intercalate ", " $ map (view (watchName . unWatchName)) watches
