{-# LANGUAGE OverloadedStrings #-}
module Utils.Vigilance.TypesSpec (spec) where

import Test.QuickCheck.Property.Common.Internal (Equal) -- bah no
import Data.Aeson

import SpecHelper

spec :: Spec
spec = parallel $ do
  describe "Monoid WatchState" $ do
    prop "obeys the law" $
      property $ eq $ prop_Monoid (T :: T WatchState)
    prop "chooses the latter unless paused" $ \a b ->
      let result = a <> b
      in case b of
        Paused -> result == a
        _      -> result == b
  describe "Monoid Config" $ do
    it "has reasonable defaults" $
      mempty `shouldBe` Config "$(HOME)/.vigilance/state/AppState"
                               Nothing
                               3000
                               (LogCfg "$(HOME)/.vigilance/vigilance.log" False)
                               []
                               3

    prop "obeys the law" $
      property $ eq $ prop_Monoid (T :: T Config)

    prop "it chooses explicit acid path over nothing" $ \path ->
      let config' = mempty <> mempty & configAcidPath .~ path :: Config
      in config' ^. configAcidPath == path

    prop "it chooses explicit email over nothing" $ \email ->
      let config' = mempty <> mempty & configFromEmail .~ Just email :: Config
      in config' ^. configFromEmail == Just email

    prop "it chooses left non-default log path over default right" $ \path ->
      let config' = mempty & configLogCfg . logCfgPath .~ path <> mempty :: Config
      in config' ^. configLogCfg . logCfgPath == path

    prop "it chooses right non-default log path over default left" $ \path ->
      let config' = mempty <> mempty & configLogCfg . logCfgPath .~ path :: Config
      in config' ^. configLogCfg . logCfgPath == path

  describe "json parsing" $ do
    prop "parses NewWatch roundtrip" $
      property $ eq $ propJSONParsing (T :: T NewWatch)

    prop "parses EWatch roundtrip" $
      property $ eq $ propJSONParsing (T :: T EWatch)

    prop "parses FailedNotification roundtrip" $
      property $ eq $ propJSONParsing (T :: T FailedNotification)

    prop "parses NotificationError roundtrip" $
      property $ eq $ propJSONParsing (T :: T NotificationError)

  describe "WatchInterval FromJSON" $ do
    it "parses correct values" $ do
      "[5, \"minutes\"]" `shouldParseJSON` Every 5 Minutes

    it "refuses to parse 0 values" $
      eitherDecode' "[0, \"minutes\"]" `shouldBe` (Left "interval must be > 0" :: Either String WatchInterval)
    it "refuses to parse negative values" $
      eitherDecode' "[-1, \"minutes\"]" `shouldBe` (Left "interval must be > 0" :: Either String WatchInterval)

propJSONParsing :: (Show a, FromJSON a, ToJSON a) => T a -> a -> Equal (Either String a)
propJSONParsing T x = parsed .==. Right x
  where parsed =  eitherDecode' str
        str    = encode x
