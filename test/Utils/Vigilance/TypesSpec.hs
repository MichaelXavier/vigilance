module Utils.Vigilance.TypesSpec (spec) where

import Test.QuickCheck.Property.Common.Internal (Equal) -- bah no
import Data.Aeson

import SpecHelper

spec :: Spec
spec = do
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
      mempty `shouldBe` Config "state/AppState" Nothing "log/vigilance.log"

    prop "obeys the law" $
      property $ eq $ prop_Monoid (T :: T Config)

    prop "it chooses explicit acid path over nothing" $ \path ->
      let config' = mempty <> mempty & configAcidPath .~ path :: Config
      in config' ^. configAcidPath == path

    prop "it chooses explicit email over nothing" $ \email ->
      let config' = mempty <> mempty & configFromEmail .~ Just email :: Config
      in config' ^. configFromEmail == Just email

    prop "it chooses left non-default log path over default right" $ \path ->
      let config' = mempty & configLogPath .~ path <> mempty :: Config
      in config' ^. configLogPath == path

    prop "it chooses right non-default log path over default left" $ \path ->
      let config' = mempty <> mempty & configLogPath .~ path :: Config
      in config' ^. configLogPath == path

  describe "json parsing" $ do
    prop "parses NewWatch roundtrip" $
      property $ eq $ propJSONParsing (T :: T (NewWatch))

    prop "parses EWatch roundtrip" $
      property $ eq $ propJSONParsing (T :: T (EWatch))


propJSONParsing :: (Show a, FromJSON a, ToJSON a) => T a -> a -> Equal (Either String a)
propJSONParsing T x = parsed .==. Right x
  where parsed =  eitherDecode' str
        str    = encode x
