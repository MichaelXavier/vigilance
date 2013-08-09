{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils.Vigilance.ConfigSpec (spec) where

import ClassyPrelude
import SpecHelper

import Utils.Vigilance.Config

spec :: Spec
spec = do
  describe "loadConfig" $ do
    it "always generates a logger" $ do
      "empty_config.conf" `shouldParseConfig` mempty

p `shouldParseConfig` cfg = loadConfig path `shouldReturn` cfg
  where path = show ("test" </> "fixtures" </> p)
