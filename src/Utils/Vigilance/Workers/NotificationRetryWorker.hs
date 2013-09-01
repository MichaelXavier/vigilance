{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils.Vigilance.Workers.NotificationRetryWorker ( runWorker ) where

import ClassyPrelude
import Control.Lens

import Utils.Vigilance.TableOps
import Utils.Vigilance.Types
import Utils.Vigilance.Utils (concatMapM)

-- | Intended to be exclusive
runWorker :: AcidState AppState -> NotifierGroup -> LogCtxT IO ()
runWorker acid notifiers = renameLogCtx "Notification Retry Worker" $ do
  fails  <- lift $ allFailedNotificationsS acid
  fails' <- catMaybes <$> mapM (notify notifiers) fns
  lift $ setFailedNotificationsS acid fails'

notify :: NotifierGroup -> FailedNotification -> LogCtxT IO (Maybe FailedNotification)
notify = undefined
