{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Utils.Vigilance.Workers.NotificationRetryWorker ( runWorker
                                                       , failuresToRetry
                                                       , notifyOrBump
                                                       , renderFail ) where

import ClassyPrelude
import Control.Lens
import Data.Acid (AcidState)
import Data.String.Conversions (ST)
import Text.InterpolatedString.Perl6 (qc)
import Utils.Vigilance.Logger ( pushLog
                              , renameLogCtx
                              , vLog )
import Utils.Vigilance.TableOps
import Utils.Vigilance.Types

-- | Intended to be exclusive
runWorker :: AcidState AppState -> Int -> NotifierGroup -> LogCtxT IO ()
runWorker acid maxRetries notifiers = renameLogCtx "Notification Retry Worker" $ do
  fails  <- lift $ allFailedNotificationsS acid
  unless (null fails) $ vLog [qc|Retrying failed notifications for {startLog fails}|]
  fails' <- catMaybes <$> mapM (notify notifiers) fails
  mapM_ logFail fails'
  let toRetry = failuresToRetry maxRetries fails'
  lift $ setFailedNotificationsS acid toRetry
  where
    startLog :: [FailedNotification] -> ST
    startLog fails = intercalate ", " $ map (\w -> w ^. failedWatch . watchName . unWatchName) fails

notifyOrBump :: Notifier -> FailedNotification -> LogCtxT IO (Maybe FailedNotification)
notifyOrBump n fn = do
  vLog logMsg
  fn' <- listToMaybe <$> n [watch]
  maybe retrySuccessful retryFailed fn'
  where retrySuccessful = return Nothing
        retryFailed fn' = return . Just $ fn' & retries .~ (fn ^. retries + 1)
        watch           = fn ^. failedWatch
        wn              = watch ^. watchName . unWatchName
        logMsg = [qc|Retrying notification {fn ^. failedPref} for {wn} after {fn ^. retries} retries|]

notify :: NotifierGroup -> FailedNotification -> LogCtxT IO (Maybe FailedNotification)
notify NotifierGroup { _ngEmail = Just n}
       fn@FailedNotification { _failedPref = (EmailNotification _)} = notifyOrBump (_emailNotifier n) fn
notify NotifierGroup {_ngHTTP}
       fn@FailedNotification { _failedPref = (HTTPNotification _)} = notifyOrBump (_httpNotifier _ngHTTP) fn
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

failuresToRetry :: Int -> [FailedNotification] -> [FailedNotification]
failuresToRetry maxRetries = filter underLimit
  where underLimit fn = fn ^. retries < maxRetries
