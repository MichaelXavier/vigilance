module Utils.Vigilance.ConfigSpec (spec) where

import SpecHelper

import Utils.Vigilance.Config

spec :: Spec
spec = do
  describe "configNotifiers" $ do
    it "always generates a logger" $ pending
