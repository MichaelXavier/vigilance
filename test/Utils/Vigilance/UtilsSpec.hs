module Utils.Vigilance.UtilsSpec (spec) where

import Utils.Vigilance.Utils

import SpecHelper

spec :: Spec
spec = parallel $ do
  describe "watchIntervalSeconds" $ do
    it "converts seconds" $ watchIntervalSeconds (Every 3 Seconds) `shouldBe` 3
    it "converts minutes" $ watchIntervalSeconds (Every 3 Minutes) `shouldBe` 180
    it "converts hours"   $ watchIntervalSeconds (Every 3 Hours) `shouldBe` 10800
    it "converts days"    $ watchIntervalSeconds (Every 3 Days) `shouldBe` 259200
    it "converts weeks"   $ watchIntervalSeconds (Every 3 Weeks) `shouldBe` 1814400
    it "converts years"   $ watchIntervalSeconds (Every 3 Years) `shouldBe` 94608000
