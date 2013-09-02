{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Utils.Vigilance.Workers.NotificationRetryWorker ( runWorker
                                                       , renderFail ) where

import ClassyPrelude
import Control.Lens
import Data.Acid (AcidState)
import Text.InterpolatedString.Perl6 (qc)
import Utils.Vigilance.Logger ( pushLog
                              , renameLogCtx
                              , vLog )
import Utils.Vigilance.TableOps
import Utils.Vigilance.Types
import Utils.Vigilance.Utils (concatMapM)

-- | Intended to be exclusive
runWorker :: AcidState AppState -> NotifierGroup -> LogCtxT IO ()
runWorker acid notifiers = renameLogCtx "Notification Retry Worker" $ do
  fails  <- lift $ allFailedNotificationsS acid
  fails' <- catMaybes <$> mapM (notify notifiers) fails
  mapM_ logFail fails'
  --TODO: reject excessive retries, configurable
  lift $ setFailedNotificationsS acid fails'

notifyOrBump :: Notifier -> FailedNotification -> LogCtxT IO (Maybe FailedNotification)
notifyOrBump n fn = do
  vLog logMsg
  fn' <- listToMaybe <$> n [watch]
  maybe retrySuccessful retryFailed fn'
  where retrySuccessful = return Nothing
        retryFailed fn' = return . Just $ fn' & retries +~ 1
        watch           = fn ^. failedWatch
        wn              = watch ^. watchName
        logMsg = [qc|Retrying notification {fn ^. failedPref} for {wn} after {fn ^. retries} retries|]

notify :: NotifierGroup -> FailedNotification -> LogCtxT IO (Maybe FailedNotification)
notify NotifierGroup { _ngEmail = Just n}
       fn@FailedNotification { _failedPref = (EmailNotification _)} = notifyOrBump (n ^. notifier) fn
notify NotifierGroup {_ngHTTP}
       fn@FailedNotification { _failedPref = (HTTPNotification _)} = notifyOrBump (_ngHTTP ^. notifier) fn
notify _ fn = pushLog [qc|No notifier configured for {fn}|] >> return Nothing

logFail :: FailedNotification -> LogCtxT IO ()
logFail = pushLog . renderFail

renderFail :: FailedNotification -> Text
renderFail FailedNotification {..} = [qc|Watch {wn} failed to notify after {_retries} retries on {pref}: {_failedLastError}|]
  where wn = _failedWatch ^. watchName . unWatchName
        pref = renderPref _failedPref

renderPref :: NotificationPreference -> Text
renderPref (EmailNotification (EmailAddress a)) = [qc|EmailNotification {a}|]
renderPref (HTTPNotification u)                 = [qc|HTTPNotification {u}|]
