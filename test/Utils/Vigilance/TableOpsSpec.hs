{-# LANGUAGE OverloadedStrings #-}
module Utils.Vigilance.TableOpsSpec (spec) where

import Control.Monad.State
import Data.Acid.Memory.Pure
import qualified Data.Table as T
import Utils.Vigilance.TableOps
import SpecHelper

spec :: Spec
spec = do
  describe "table properties" $ do
    prop "it deletes data that is known" $ \w ->
      let (w', table) = createWatch w emptyTable
      in deleteWatch (w' ^. watchId) table == emptyTable
    prop "data is findable after insert" $ \w ->
      let (w', table) = createWatch w emptyTable
      in findWatch (w' ^. watchId) table == Just w'
    prop "watchLens finds records" $ \w newInterval ->
      let (w', table) = createWatch w emptyTable
          wId         = w' ^. watchId
          table'      = watchLens (watchInterval .~ newInterval) wId table
      in (view watchInterval <$> findWatch wId table') == Just newInterval
  describe "pauseWatch" $ do
    prop "it sets the state and nothing else" $ \w ->
      let (w', table) = createWatch w emptyTable
          wid         = w' ^. watchId
          table'      = pauseWatch wid table
      in findWatch wid table' == Just (w' { _watchWState = Paused })
  describe "checkInWatch" $ do
    prop "leaves paused watches unaltered" $ \w time ->
      let (w', table) = createWatch w { _watchWState = Paused } emptyTable
          wid         = w' ^. watchId
          table'      = checkInWatch time wid table
      in findWatch wid table' == Just w'

    prop "updates non-paused watches to active with the given time" $ \w time ->
      let ws = case w ^. watchWState of
                 Paused -> Notifying
                 x      -> x
          (w', table) = createWatch (w & watchWState .~ ws) emptyTable
          wid         = w' ^. watchId
          table'      = checkInWatch time wid table
      in findWatch wid table' == Just (w' & watchWState .~ Active time)

  describe "acid events" $ do
    let acid = openAcidState $ AppState mempty

    prop "it inserts correctly" $ \w ->
      let (acid', w') = update acid $ insert w
          result      = query acid' $ find w' 
      in result == Just (w & watchId .~ 1)

    prop "it deletes data that is known" $ \w ->
      let (acid', w') = update acid $ insert w
          (acid'')    = update_ acid' $ delete w'
          result      = query acid'' $ find w'
      in  result == Nothing


  describe "sweepTable" $ do
    prop "does not reduce or increase the size of the table" $ \watches t ->
      let table = fromList watches
      in T.count (sweepTable t table) == length watches
    it "derp" $
      let watches = [Watch {_watchId = (), _watchName = ";", _watchInterval = Every 2 Days, _watchWState = Paused, _watchNotifications = [EmailNotification (EmailAddress {_unEmailAddress = "8\DEL?]"})]},Watch {_watchId = (), _watchName = "yHAU", _watchInterval = Every 2 Seconds, _watchWState = Paused, _watchNotifications = [EmailNotification (EmailAddress {_unEmailAddress = "[ ]"})]}]
          table   = fromList watches
          t       = 5
      --in T.count (sweepTable t table) `shouldBe` length watches
      in T.count table `shouldBe` length watches
  where insert = CreateWatchEvent
        delete = DeleteWatchEvent . view watchId
        find   = FindWatchEvent . view watchId
