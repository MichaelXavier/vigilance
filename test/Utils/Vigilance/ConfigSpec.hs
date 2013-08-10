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

p `shouldParseConfig` cfg = loadConfig path `shouldReturn` cfg
  where path = unpack ("test" </> "fixtures" </> p)

fullConfig :: Config
fullConfig = Config "ACIDPATH" (Just . EmailAddress $ "vigilance@example.com") 9000 "LOGPATH"

partialConfig :: Config
partialConfig = Config "ACIDPATH" (Just . EmailAddress $ "vigilance@example.com") defaultPort defaultLogPath
