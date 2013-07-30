{-# LANGUAGE OverloadedStrings #-}
module Utils.Vigilance.Notifiers.EmailSpec (spec) where

import Data.List (nub)
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
      in nub fromEmails == ["foo@bar.com"]


ctx :: EmailContext
ctx = EmailContext $ EmailAddress "foo@bar.com"
