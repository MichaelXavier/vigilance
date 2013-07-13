module Utils.Vigilance.TypesSpec (spec) where

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
