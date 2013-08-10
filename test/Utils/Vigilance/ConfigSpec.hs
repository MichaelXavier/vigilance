{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils.Vigilance.ConfigSpec (spec) where

import ClassyPrelude
import SpecHelper

import Utils.Vigilance.Config

spec :: Spec
spec = do
  describe "loadConfig" $ do
    it "parses the empty config" $ do
      "empty_config.conf" `shouldParseConfig` mempty

    it "parses the full config" $ do
      "full_config.conf" `shouldParseConfig` fullConfig

    it "merges partial configs into the default config" $ do
      "partial_config.conf" `shouldParseConfig` partialConfig

    it "parses NewWatches if they are specified" $ do
      "config_with_watches.conf" `shouldParseConfig` configWithWatches

p `shouldParseConfig` cfg = loadConfig path `shouldReturn` cfg
  where path = unpack ("test" </> "fixtures" </> p)

fullConfig :: Config
fullConfig = Config "ACIDPATH" (Just . EmailAddress $ "vigilance@example.com") 9000 "LOGPATH" []

partialConfig :: Config
partialConfig = Config "ACIDPATH" (Just . EmailAddress $ "vigilance@example.com") defaultPort defaultLogPath []

configWithWatches :: Config
configWithWatches = Config "ACIDPATH" (Just . EmailAddress $ "vigilance@example.com") defaultPort defaultLogPath [w1, w2]
  where w1 = Watch () "foo" (Every 2 Seconds) mempty [ EmailNotification . EmailAddress $ "notify@example.com"
                                                     , EmailNotification . EmailAddress $ "notify2@example.com"]
        w2 = Watch () "bar" (Every 3 Minutes) mempty []
