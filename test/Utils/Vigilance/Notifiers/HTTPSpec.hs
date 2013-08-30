module Utils.Vigilance.Notifiers.HTTPSpec (spec) where

import Utils.Vigilance.Notifiers.HTTP
import Debug.Trace (traceShow)
import SpecHelper

spec :: Spec
spec = parallel $ do
  describe "watchesWithNotifications" $ do
    it "returns nothing on an empty list" $
      watchesWithNotifications [] `shouldBe` []
    prop "returns an entry for each url in the watches" $ \watches ->
      let notifications = concatMap (view watchNotifications) watches
          withURLS = filter isURLNotification notifications
          withNotifications = watchesWithNotifications watches
      in length withURLS == length withNotifications


isURLNotification :: NotificationPreference -> Bool
isURLNotification (HTTPNotification _) = True
isURLNotification _                    = False
