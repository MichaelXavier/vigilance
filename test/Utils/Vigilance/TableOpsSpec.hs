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
  describe "unPauseWatch" $ do
    prop "sets an non-active watch to active" $ \w t ->
      let ws = case w ^. watchWState of
                 Active _ -> Notifying
                 x        -> x
          (w', table) = createWatch (w & watchWState .~ ws) emptyTable
          wid         = w' ^. watchId
          table'      = unPauseWatch t wid table
      in findWatch wid table' == Just (w' { _watchWState = Active t })
    prop "leaves an active watch alone" $ \w currentT newT ->
      let (w', table) = createWatch (w & watchWState .~ Active currentT) emptyTable
          wid         = w' ^. watchId
          table'      = unPauseWatch newT wid table
      in findWatch wid table' == Just w'

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

  describe "getNotifying" $ do
    prop "returns empty list on a table without notifying watches" $ \watches ->
      let fixState Notifying = Paused
          fixState x         = x
          table              = fromList $ map (\w -> w & watchWState %~ fixState) watches
      in getNotifying table == []

    prop "returns only the notifying events" $ \watches n ->
      let fixState Notifying = Paused
          fixState x         = x
          notNotifying       = map (\w -> w & watchWState %~ fixState) watches
          notifying          = replicate n $ baseNewWatch & watchWState .~ Notifying
          table              = fromList $ shuffleIn notNotifying notifying
          result             = getNotifying table
      in map removeId result == notifying

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

    prop "does nothing to an empty table" $ \t ->
      (sweepTable t emptyTable) == emptyTable

    it "does nothing when the items don't need to be sweeped" $
      let table = fromList [baseNewWatch]
      in (sweepTable 123 table) == table

    it "sweeps expired watches" $
      let w           = baseNewWatch & watchWState .~ (Active 123)
          (w', table) = createWatch w emptyTable
          table'      = sweepTable 125 table
          w''         = findWatch (w' ^. watchId) table'
          state'      = _watchWState <$> w''
      in state' `shouldBe` Just Notifying

  where insert = CreateWatchEvent
        delete = DeleteWatchEvent . view watchId
        find   = FindWatchEvent . view watchId

shuffleIn :: [a] -> [a] -> [a]
shuffleIn xs = concatMap collapse . zip xs
  where collapse (x, y) = [x, y]

removeId :: EWatch -> NewWatch
removeId w = w & watchId .~ ()
