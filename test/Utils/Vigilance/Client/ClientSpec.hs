{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Utils.Vigilance.ConfigSpec (spec) where

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


fullWatch :: EWatch
fullWatch = baseWatch { _watchWState = Active 12345
                      , _watchNotifications = [n1, n2] }
  where n1 = EmailNotification $ EmailAddress "foo@example.com"
        n2 = EmailNotification $ EmailAddress "bar@example.com"

fullWatchText :: Text
fullWatchText = [qc|whatever (1) - Every 1 Seconds - Active 12345s

Notifications:
 - Email: foo@example.com
 - Email: bar@example.com|]
