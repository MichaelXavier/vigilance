{-# LANGUAGE OverloadedStrings #-}
module Utils.Vigilance.Notifiers.EmailSpec (spec) where

import Network.Mail.Mime ( Address(..)
                         , Mail(..))
import SpecHelper

import Utils.Vigilance.Notifiers.Email

spec :: Spec
spec = do
  describe "generateEmails" $ do
    prop "it always uses the context's from" $ \(NonEmpty watches) email ->
      let watches'   = map (\w -> w & watchNotifications <>~ [EmailNotification email]) watches
          fromEmails = map (addressEmail . mailFrom) $ generateEmails watches' ctx
      in all (== "foo@bar.com") fromEmails
    it "groups into emails by watch preferences" $
      let watches = [ watchForEmails ["foo@bar.com", "bar@baz.com"]
                    , watchForEmails ["bar@baz.com"] ]
          emails  = generateEmails watches ctx
      in map (map addressEmail . mailTo) emails `shouldBe` [["bar@baz.com"], ["foo@bar.com"]]


ctx :: EmailContext
ctx = EmailContext $ EmailAddress "foo@bar.com"

watchForEmails :: [Text] -> EWatch
watchForEmails emails = baseWatch & watchNotifications <>~ notifications
  where notifications = map (EmailNotification . EmailAddress) emails
