moudle Utils.Vigilance.ConfigSpec (spec) where

import SpecHelper

import Utils.Vigilance.Config

spec :: Spec
spec = do
  describe "configNotifiers" $ do
    it "always generates a logger" $ 


baseConfig :: Config
baseConfig = Config Nothing Nothing
