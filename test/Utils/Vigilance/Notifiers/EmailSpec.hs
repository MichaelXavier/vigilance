{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Utils.Vigilance.Notifiers.EmailSpec (spec) where

import Prelude (head)
import ClassyPrelude
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
    it "groups into emails by watch preferences" $
      let watch = watchForEmails ["bar@baz.com"]
          email:_  = generateEmails [watch] ctx
      in "- whatever (Every 1 Seconds)" `shouldBeIncludedInBodyOf` email

a `shouldBeIncludedInBodyOf` m = a `shouldBeIncludedIn` body
  where body = decodeUtf8 . partContent . head . concat . mailParts $ m

a `shouldBeIncludedIn` b = shouldSatisfy b (a `isInfixOf`)

ctx :: EmailContext
ctx = EmailContext $ EmailAddress "foo@bar.com"

watchForEmails :: [Text] -> EWatch
watchForEmails emails = baseWatch & watchNotifications <>~ notifications
  where notifications = map (EmailNotification . EmailAddress) emails
