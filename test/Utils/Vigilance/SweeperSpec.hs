{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Utils.Vigilance.SweeperSpec (spec) where

import SpecHelper

import Utils.Vigilance.Sweeper

spec :: Spec
spec = do
  describe "expired" $ do
    prop "inactive ewatch always false" $ \(w :: EWatch) t ->
      let ws = case w ^. watchWState of
                 Active _ -> Notifying
                 x        -> x
      in expired t (w & watchWState .~ ws) == False

    let t = 123 :: POSIXTime
    let w = baseWatch & watchWState .~ Active t

    it "returns True on expired by second" $
      expired t (w & watchWState %~ bumpTime (-2)) `shouldBe` True

    it "returns True on expired by minute" $
      let w' = w & watchInterval .~ (Every 1 Minutes)
      in expired t (w' & watchWState %~ bumpTime (-61)) `shouldBe` True

    it "returns True on expired by hour" $
      let w' = w & watchInterval .~ (Every 1 Hours)
      in expired t (w' & watchWState %~ bumpTime (-3601)) `shouldBe` True

    it "returns True on expired by day" $
      let w' = w & watchInterval .~ (Every 1 Days)
      in expired t (w' & watchWState %~ bumpTime (-86401)) `shouldBe` True

    it "returns True on expired by week" $
      let w' = w & watchInterval .~ (Every 1 Weeks)
      in expired t (w' & watchWState %~ bumpTime (-604801)) `shouldBe` True

    it "returns True on expired by year" $
      let w' = w & watchInterval .~ (Every 1 Years)
      in expired t (w' & watchWState %~ bumpTime (-31536001)) `shouldBe` True

    it "returns False on expired by second" $
      expired t w `shouldBe` False

    it "returns False on expired by minute" $
      let w' = w & watchInterval .~ (Every 1 Minutes)
      in expired t (w' & watchWState %~ bumpTime (-58)) `shouldBe` False

    it "returns False on expired by hour" $
      let w' = w & watchInterval .~ (Every 1 Hours)
      in expired t (w' & watchWState %~ bumpTime (-3598)) `shouldBe` False

    it "returns False on expired by day" $
      let w' = w & watchInterval .~ (Every 1 Days)
      in expired t (w' & watchWState %~ bumpTime (-86398)) `shouldBe` False

    it "returns False on expired by week" $
      let w' = w & watchInterval .~ (Every 1 Weeks)
      in expired t (w' & watchWState %~ bumpTime (-604798)) `shouldBe` False

    it "returns False on expired by year" $
      let w' = w & watchInterval .~ (Every 1 Years)
      in expired t (w' & watchWState %~ bumpTime (-31535998)) `shouldBe` False

    it "handles expired w/ multiples properly" $
      let w' = w & watchInterval .~ (Every 5 Seconds)
      in expired t (w & watchWState %~ bumpTime (-6)) `shouldBe` True

    it "handles not expired w/ multiples properly" $
      let w' = w & watchInterval .~ (Every 5 Seconds)
      in expired t (w & watchWState %~ bumpTime (-4)) `shouldBe` True

  describe "sweepWatch" $ do
    let t = 123
    let w = baseWatch & watchWState .~ Active t

    prop "does not affect unexpired watches" $ \time ->
      sweepWatch time baseWatch == baseWatch

    it "sweeps expired watches" $
      let w' = w & watchWState %~ bumpTime (-2)
      in sweepWatch t w' ^. watchWState `shouldBe` Notifying
