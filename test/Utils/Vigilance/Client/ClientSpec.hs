{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Utils.Vigilance.Client.ClientSpec (spec) where

import ClassyPrelude
import SpecHelper

import Utils.Vigilance.Client.Client


spec :: Spec
spec = parallel $ do
  describe "renderWatch" $ do
    it "renders the watch correctly" $
      renderWatch baseWatch `shouldBe` "whatever (1) - Every 1 Seconds - Paused"

  describe "renderWatchInfo" $ do
    it "renders the full watch info" $
      renderWatchInfo fullWatch `shouldBe` fullWatchText

  describe "renderFailedNotifications" $ do
    it "renders a message with no errors" $
      renderFailedNotifications [] `shouldBe` "All notifications sent successfully."
    it "renders a line for each error" $
      let fn' = baseFN & failedPref .~ (EmailNotification $ EmailAddress "foo@example.com")
                       & failedLastError .~ (FailedByException "oh snap")
      in renderFailedNotifications [baseFN, fn'] `shouldBe` failedNotificationsMessage


fullWatch :: EWatch
fullWatch = baseWatch { _watchWState = Active 12345
                      , _watchNotifications = [n1, n2] }
  where n1 = EmailNotification $ EmailAddress "foo@example.com"
        n2 = HTTPNotification  "http://example.com"


failedNotificationsMessage :: Text
failedNotificationsMessage = [qc|The following errors were encountered when testing:
- HTTP Notification (example.com) for whatever (1): Failed with status code 500
- Email Notification (foo@example.com) for whatever (1): Failed with exception "oh snap"|]

fullWatchText :: Text
fullWatchText = [qc|whatever (1) - Every 1 Seconds - Active 12345s

Notifications:
 - Email: foo@example.com
 - HTTP: http://example.com|]
