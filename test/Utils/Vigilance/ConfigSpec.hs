{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils.Vigilance.ConfigSpec (spec) where

import ClassyPrelude
import SpecHelper

import Utils.Vigilance.Config

spec :: Spec
spec = parallel $ do
  describe "loadConfig" $ do
    it "parses the empty config" $ do
      "empty_config.conf" `shouldParseConfig` mempty

    it "parses the full config" $ do
      "full_config.conf" `shouldParseConfig` fullConfig

    it "merges partial configs into the default config" $ do
      "partial_config.conf" `shouldParseConfig` partialConfig

    it "parses NewWatches if they are specified" $ do
      "config_with_watches.conf" `shouldMatchWatchesBesidesTime` configWithWatches

    it "parses NewWatches imported from other files" $ do
      "config_with_watch_imports.conf" `shouldMatchWatchesBesidesTime` configWithWatches

p `shouldParseConfig` cfg = loadConfig path `shouldReturn` cfg
  where path = unpack ("test" </> "fixtures" </> p)

p `shouldMatchWatchesBesidesTime` cfg = (wrapWatches <$> loadConfig path) `shouldReturn` (wrapWatches cfg)
  where path = unpack ("test" </> "fixtures" </> p)
        wrapWatches c = map LaxWatchMatcher $ c ^. configWatches

newtype LaxWatchMatcher = LaxWatchMatcher NewWatch deriving (Show)

instance Eq LaxWatchMatcher where
  (LaxWatchMatcher a) == (LaxWatchMatcher b) = (a & watchWState %~ cleanState) == (b & watchWState %~ cleanState)
    where cleanState (Active _) = Active 0
          cleanState s          = s

fullConfig :: Config
fullConfig = Config "ACIDPATH" (Just . EmailAddress $ "vigilance@example.com") 9000 logCfg []
  where logCfg = LogCfg "LOGPATH" True

partialConfig :: Config
partialConfig = Config "ACIDPATH" (Just . EmailAddress $ "vigilance@example.com") defaultPort defaultLogCfg []

configWithWatches :: Config
configWithWatches = Config "ACIDPATH" (Just . EmailAddress $ "vigilance@example.com") defaultPort defaultLogCfg [w1, w2]
  where w1 = Watch () "foo" (Every 2 Seconds) (Active 0) [ EmailNotification . EmailAddress $ "notify@example.com"
                                                     , EmailNotification . EmailAddress $ "notify2@example.com"]
        w2 = Watch () "bar" (Every 3 Minutes) (Active 0) []
