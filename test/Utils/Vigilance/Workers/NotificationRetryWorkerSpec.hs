{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Utils.Vigilance.Workers.NotificationRetryWorkerSpec (spec) where

import ClassyPrelude
import Control.Lens

import SpecHelper
import Utils.Vigilance.Logger ( runInLogCtx
                              , createLogChan )
import Utils.Vigilance.Workers.NotificationRetryWorker

spec :: Spec
spec = parallel $ do
  describe "renderFail" $ do
    it "renders EmailNotification" $
      renderFail emailFail `shouldBe` "Watch whatever failed to notify after 2 retries on EmailNotification foo@example.com: FailedByException \"crap\""
    it "renders HTTPNotification" $
      renderFail httpFail `shouldBe` "Watch whatever failed to notify after 2 retries on HTTPNotification example.com: FailedByCode 500"
  describe "failuresToRetry" $ do
    it "removes fns that are on the border or over" $
      let ok      = baseFN
          limited = baseFN & retries .~ 2
          tooMany = baseFN & retries .~ 3
      in failuresToRetry 2 [ok, limited, tooMany] `shouldBe` [ok]
  describe "notifyOrBump" $ do
    let fn = baseFN & retries .~ 2

    describe "with no errors" $ do
      let notifier = bogusNotifier []

      it "returns Nothing" $ do
        rl (notifyOrBump notifier fn) `shouldReturn` Nothing
      it "logs the right messages" $ do
        (_, logs) <- rl' $ notifyOrBump notifier fn
        logs `shouldBe` [VerboseLogMessage "[test] Retrying notification HTTPNotification \"example.com\" for whatever after 2 retries\n"]

    describe "with errors" $ do
      let notifier = bogusNotifier [emailFail & retries .~ 0]

      it "modifies the reported failure by incrementing the current counter, disregarding the returned counter" $ do
        rl (notifyOrBump notifier fn) `shouldReturn` Just (emailFail & retries .~ 3)

    describe "notifier returns multiple fails (shouldn't happen)" $ do
      let notifier = bogusNotifier [emailFail & retries .~ 0, httpFail]

      it "uses the first one" $ do
        rl (notifyOrBump notifier fn) `shouldReturn` Just (emailFail & retries .~ 3)


emailFail :: FailedNotification
emailFail = httpFail & update
  where update = (failedPref .~ (EmailNotification $ EmailAddress "foo@example.com")) . (failedLastError .~ FailedByException "crap")

httpFail :: FailedNotification
httpFail = baseFN & retries .~ 2

bogusNotifier :: [FailedNotification] -> Notifier
bogusNotifier fns = const $ return fns

rl :: (LogCtxT IO a) -> IO a
rl action = fst <$> rl' action

rl' :: (LogCtxT IO a) -> IO (a, [LogMessage])
rl' action = do
  logChan <- createLogChan
  res     <- runInLogCtx (LogCtx "test" logChan) action
  logs    <- concat <$> collectLogs logChan
  return (res, reverse logs)
  where collectLogs logChan = unfoldM (atomically $ tryReadTChan logChan)
