{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Utils.Vigilance.TableOpsSpec (spec) where

import ClassyPrelude hiding (toList, fromList)
import Control.Monad.State
import Data.Acid.Memory.Pure
import qualified Data.Store as S
import Data.Store (toList,  (.==), size, elements)
import Data.Store.Lens (with)
import Utils.Vigilance.TableOps
import SpecHelper

import Debug.Trace

spec :: Spec
spec = parallel $ do
  describe "table properties" $ do
    prop "it deletes data that is known" $ \w ->
      let (w', table) = createWatch w emptyTable
          table'      = deleteWatch (w' ^. watchName) table
      in  size table' == 0
    prop "data is findable after insert" $ \w ->
      let (w', table) = createWatch w emptyTable
      in findWatch (w' ^. watchName) table == Just w'
    prop "watchLens finds records" $ \w newInterval ->
      let (w', table) = createWatch w emptyTable
          wId         = w' ^. watchName
          table'      = watchLens (watchInterval .~ newInterval) wId table
      in (view watchInterval <$> findWatch wId table') == Just newInterval

  describe "allWatches" $ do
    prop "it returns the contents of the table only" $ \(UniqueWatches watches) ->
      let table  = fromList watches
      in (map removeId $ allWatches table) == watches
  describe "pauseWatch" $ do
    prop "it sets the state and nothing else" $ \w ->
      let (w', table) = createWatch w emptyTable
          wName         = w' ^. watchName
          table'      = pauseWatch wName table
      in findWatch wName table' == Just (w' { _watchWState = Paused })
  describe "unPauseWatch" $ do
    prop "sets an non-active watch to active" $ \w t ->
      let ws = case w ^. watchWState of
                 Active _ -> Notifying
                 x        -> x
          (w', table) = createWatch (w & watchWState .~ ws) emptyTable
          wName         = w' ^. watchName
          table'      = unPauseWatch t wName table
      in findWatch wName table' == Just (w' { _watchWState = Active t })
    prop "leaves an active watch alone" $ \w currentT newT ->
      let (w', table) = createWatch (w & watchWState .~ Active currentT) emptyTable
          wName         = w' ^. watchName
          table'      = unPauseWatch newT wName table
      in findWatch wName table' == Just w'

  describe "checkInWatch" $ do
    prop "leaves paused watches unaltered" $ \w time ->
      let (w', table) = createWatch w { _watchWState = Paused } emptyTable
          wName         = w' ^. watchName
          table'      = checkInWatch time wName table
      in findWatch wName table' == Just w'

    prop "updates non-paused watches to active with the given time" $ \w time ->
      let ws = case w ^. watchWState of
                 Paused -> Notifying
                 x      -> x
          (w', table) = createWatch (w & watchWState .~ ws) emptyTable
          wName         = w' ^. watchName
          table'      = checkInWatch time wName table
      in findWatch wName table' == Just (w' & watchWState .~ Active time)
  describe "getNotifying" $ do
    prop "returns empty list on a table without notifying watches" $ \watches ->
      let fixState Notifying = Paused
          fixState x         = x
          table              = fromList $ map (\w -> w & watchWState %~ fixState) watches
      in getNotifying table == []

    prop "returns only the notifying events" $ \(UniqueWatches watches) ->
      let fixState Notifying = Paused
          fixState x         = x
          notNotifying       = map (\w -> w & watchWState %~ fixState) watches
          aNotifying         = baseNewWatch & watchWState .~ Notifying
          notifying          :: [NewWatch]
          notifying          = [aNotifying & watchName .~ "foo", aNotifying & watchName .~ "bar"]
          table              = fromList $ shuffleIn notNotifying notifying
          result             = getNotifying table
      in (map removeId result) == notifying

    it "is queryable after notifying" $
      let watches = [Watch {_watchId = (), _watchName = "foo", _watchInterval = Every 2 Seconds, _watchWState = Active 123, _watchNotifications = []}]
          table   = fromList watches
          table'  = sweepTable 999 table
          result  = map snd $ S.lookup (sWatchWState .== Notifying) table'
          expectedResult = map (\w -> w & watchWState .~ Notifying) watches
      in result `shouldBe` expectedResult

  -- dog slow
  --TODO: pretty sure there's a better way to restrict length than take
  describe "completeNotifying" $ do
    prop "it does nothing when given bogus ids" $ \(UniqueWatches watches) names ->
      let watches'   = take 10 watches
          table      = fromList watches' :: WatchTable
          bogusNames = (nub names) \\ (map _watchName watches')
          table'     = completeNotifying bogusNames table :: WatchTable
      in table' `equalsTable` table

    prop "it does nothing when no ids" $ \watches ->
      let table  = fromList $ take 10 watches
          table' = completeNotifying [] table
      in table' `equalsTable` table

    prop "it only triggers watches that are in the id list AND notifying" $ \(UniqueWatches watches) ->
      let notifying        = filter (\w -> w ^. watchWState == Notifying) watches
          alreadyTriggered = filter (\w -> w ^. watchWState == Triggered) watches
          table        = fromList watches
          allNames     = tableNames table
          table'       = completeNotifying allNames table
          triggerNames = sort $ map (view watchName) $ table' ^. with (sWatchWState .== Triggered) . to elements --christ
          expectedTriggerNames = sort $ map (view watchName) $ notifying ++ alreadyTriggered
      in triggerNames == expectedTriggerNames

    prop "notifying leaves no remaining watches notifying when all specified" $ \(UniqueWatches watches) ->
      let notifying    = filter (\w -> w ^. watchWState == Notifying) watches
          table        = fromList watches
          allNames     = tableNames table
          table'       = completeNotifying allNames table
      in getNotifying table' == []

  describe "mergeStaticWatches" $ do
    prop "no watches to merge clears the table" $ \watches ->
      let table = mergeStaticWatches [] $ fromList watches
      in S.size table `shouldBe` 0

    prop "it only updates watchInterval and watchNotifications" $ \(UniqueWatches watches) targetWatch wInterval' wState' notifications' ->
      let table = fromList (watches ++ [targetWatch])
          targetWatch' = targetWatch { _watchInterval = wInterval', _watchWState = wState', _watchNotifications = notifications' } 
          table' = mergeStaticWatches [targetWatch'] table
          expectedResult   = targetWatch { _watchInterval = wInterval', _watchNotifications = notifications' }
          resultingWatches = table' ^. with (sWatchName .== (targetWatch' ^. watchName)) . to elements
      in (map removeId $ allWatches table') == [expectedResult]

    prop "leaves only the targeted watches" $ \(UniqueWatches existing) (UniqueWatches newWatches) ->
      let table        = fromList existing
          newNames     = sort $ map (view watchName) newWatches
          table'       = mergeStaticWatches newWatches table
          namesInTable = sort $ map (view watchName . removeId) $ allWatches table'
      in namesInTable == newNames

    it "retains watch state on new watches" $
      let table       = fromList []
          table'      = mergeStaticWatches [baseNewWatch { _watchWState = Triggered }] table
          newWatchStates = map (view watchWState . removeId) $ allWatches table'
      in newWatchStates == [Triggered]

  --TODO: just use the createWatchS and the like here instead?
  describe "acid events" $ do
    let acid = openAcidState $ AppState mempty mempty

    prop "it inserts correctly" $ \w ->
      let (acid', w') = update acid $ insert w
          result      = query acid' $ find w' 
      in result == Just (w & watchId .~ 1)

    prop "it deletes data that is known" $ \w ->
      let (acid', w') = update acid $ insert w
          (acid'')    = update_ acid' $ delete w'
          result      = query acid'' $ find w'
      in  result == Nothing

    describe "addFailedNotifications Event" $ do
      it "appends failures" $
        let acid'  = update_ acid  $ AddFailedNotificationsEvent [baseFN]
            acid'' = update_ acid' $ AddFailedNotificationsEvent [baseFN]
            fns    = query acid'' AllFailedNotificationsEvent
        in fns `shouldBe` [baseFN, baseFN]

    describe "setFailedNotificationsS Event" $ do
      it "sets the full list of failures" $
        let acid'  = update_ acid  $ SetFailedNotificationsEvent [baseFN]
            fns    = query acid' AllFailedNotificationsEvent
        in fns `shouldBe` [baseFN]

      it "overwrites existing ones" $
        let acid'  = update_ acid  $ AddFailedNotificationsEvent [baseFN]
            acid'' = update_ acid  $ SetFailedNotificationsEvent []
            fns    = query acid'' AllFailedNotificationsEvent
        in fns `shouldBe` []

  describe "sweepTable" $ do
    prop "does not reduce or increase the size of the table" $ \(UniqueWatches watches) t ->
      let table = fromList watches
      in size (sweepTable t table) == length watches

    prop "does nothing to an empty table" $ \t ->
      (sweepTable t emptyTable) `equalsTable` emptyTable

    it "does nothing when the items don't need to be sweeped" $
      let table = fromList [baseNewWatch]
      in (sweepTable 123 table) `equalsTable` table

    it "sweeps expired watches" $
      let w           = baseNewWatch & watchWState .~ (Active 123)
          (w', table) = createWatch w emptyTable
          table'      = sweepTable 125 table
          w''         = findWatch (w' ^. watchName) table'
          state'      = _watchWState <$> w''
      in state' `shouldBe` Just Notifying

  where insert = CreateWatchEvent
        delete = DeleteWatchEvent . view watchName
        find   = FindWatchEvent . view watchName

shuffleIn :: [a] -> [a] -> [a]
shuffleIn (x:xs) (y:ys) = x:y:shuffleIn xs ys
shuffleIn [] ys         = ys
shuffleIn xs []         = xs

removeId :: EWatch -> NewWatch
removeId w = w & watchId .~ ()

tableNames = map (_watchName) . elements

-- sort not necessary?
t1 `equalsTable` t2 = sortedElements t1 == sortedElements t2
  where sortedElements = sort . elements
