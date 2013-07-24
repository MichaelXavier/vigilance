module Utils.Vigilance.TypesSpec (spec) where

import Test.QuickCheck.Property.Common.Internal (Equal) -- bah no
import Data.Aeson

import SpecHelper

spec :: Spec
spec = do
  describe "Monoid WatchState" $ do
    prop "obeys the law" $
      property $ eq $ prop_Monoid (T :: T (WatchState))
    prop "chooses the latter unless paused" $ \a b ->
      let result = a <> b
      in case b of
        Paused -> result == a
        _      -> result == b
  describe "json parsing" $ do
    prop "parses NewWatch roundtrip" $
      property $ eq $ propJSONParsing (T :: T (NewWatch))

    prop "parses EWatch roundtrip" $
      property $ eq $ propJSONParsing (T :: T (EWatch))


propJSONParsing :: (FromJSON a, ToJSON a) => T a -> a -> Equal (Either String a)
propJSONParsing T x = parsed .==. Right x
  where parsed = eitherDecode' str
        str    = encode x
