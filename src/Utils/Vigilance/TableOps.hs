{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
module Utils.Vigilance.TableOps ( allWatches
                                , allWatchesEvent
                                , AllWatchesEvent(..)
                                , allWatchesS
                                , createWatch
                                , createWatchEvent
                                , CreateWatchEvent(..)
                                , createWatchS
                                , deleteWatch
                                , deleteWatchEvent
                                , DeleteWatchEvent(..)
                                , deleteWatchS
                                , findWatch
                                , findWatchEvent
                                , FindWatchEvent(..)
                                , findWatchS
                                , pauseWatch
                                , pauseWatchEvent
                                , PauseWatchEvent(..)
                                , pauseWatchS
                                , unPauseWatch
                                , unPauseWatchEvent
                                , UnPauseWatchEvent(..)
                                , unPauseWatchS
                                , checkInWatch
                                , checkInWatchEvent
                                , CheckInWatchEvent(..)
                                , checkInWatchS
                                , watchLens
                                , sweepTable
                                , sweepTableEvent
                                , SweepTableEvent(..)
                                , sweepTableS
                                , getNotifying
                                , getNotifyingEvent
                                , GetNotifyingEvent(..)
                                , getNotifyingS
                                , completeNotifying
                                , completeNotifyingEvent
                                , CompleteNotifyingEvent(..)
                                , completeNotifyingS
                                , mergeStaticWatches
                                , mergeStaticWatchesEvent
                                , MergeStaticWatchesEvent(..)
                                , mergeStaticWatchesS
                                , fromList
                                , getId
                                , sWatchId
                                , sWatchName
                                , sWatchInterval
                                , sWatchWState
                                , sWatchNotifications
                                , emptyTable) where

import ClassyPrelude hiding (fromList)
import Control.Lens
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ask)
import Control.Monad.State ( get
                           , put)
import Data.Acid
import Data.Acid.Advanced (update', query')
import Data.List (foldl')
import Data.Store.Lens (with)
import Debug.Trace
import           Data.Store ((.==), (:.)(..), (.&&))
import qualified Data.Store as S
import qualified Data.Store.Storable as SS
import qualified Data.Store.Selection as SEL
import Data.Time.Clock.POSIX (POSIXTime)
import Utils.Vigilance.Sweeper (sweepWatch)
import Utils.Vigilance.Types

-- Selections
sWatchId :: (WatchStoreTag, S.N0)
sWatchId = (WatchStoreTag, S.n0)

sWatchName :: (WatchStoreTag, S.N1)
sWatchName = (WatchStoreTag, S.n1)

sWatchInterval :: (WatchStoreTag, S.N2)
sWatchInterval = (WatchStoreTag, S.n2)

sWatchWState :: (WatchStoreTag, S.N3)
sWatchWState = (WatchStoreTag, S.n3)

sWatchNotifications :: (WatchStoreTag, S.N4)
sWatchNotifications = (WatchStoreTag, S.n4)

allWatches :: WatchTable -> [EWatch]
allWatches = map ewatch . S.toList

-- Helpers

getId (wid :. _) = wid

ewatch (k, w) = w & watchId .~ getId k -- stateful?

newwatch w = w & watchId .~ ()

-- API

createWatch :: NewWatch -> WatchTable -> (EWatch, WatchTable)
createWatch w s = (w & watchId .~ getId k, s')
  where (k, s') = SS.insert' w s

deleteWatch :: WatchName -> WatchTable -> WatchTable
deleteWatch n = S.delete (sWatchName .== n)

findWatch :: WatchName -> WatchTable -> Maybe EWatch
findWatch n = listToMaybe . map ewatch . S.lookup (sWatchName .== n)

watchLens :: (NewWatch -> NewWatch) -> WatchName -> WatchTable -> WatchTable
watchLens f n table = table & with (sWatchName .== n) %~ (over mapped f)

checkInWatch :: POSIXTime -> WatchName -> WatchTable -> WatchTable
checkInWatch time = watchLens doCheckIn
  where doCheckIn w        = w & watchWState %~ updateState
        updateState Paused = Paused
        updateState _      = Active time

pauseWatch :: WatchName -> WatchTable -> WatchTable
pauseWatch = watchLens pause
  where pause w = w & watchWState .~ Paused

unPauseWatch :: POSIXTime -> WatchName -> WatchTable -> WatchTable
unPauseWatch t = watchLens unPause
  where unPause w = w & watchWState %~ updateState
        updateState (Active newTime) = Active newTime
        updateState _                = Active t

-- this is an unmitigated disaster because S.map does not reindex properly
-- https://github.com/Palmik/data-store/issues/3
sweepTable :: POSIXTime -> WatchTable -> WatchTable
sweepTable time = map' sweep
  where sweep  = sweepWatch time
        map' f = S.fromList' . map f' . S.toList
          where f' (k, w) = (S.dimO (getId k) S..: S.dimO a S..: S.dimO b  S..: S.dimO c S..:. S.dimM d, w')
                  where w'@(Watch () a b c d) = f w

getNotifying :: WatchTable -> [EWatch]
getNotifying = map ewatch . S.lookup (sWatchWState .== Notifying)


completeNotifying :: [WatchName] -> WatchTable -> WatchTable
completeNotifying [] table  = table
completeNotifying names table = SS.update' (Just . updateState) scope table
  where updateState w = w & watchWState .~ Triggered
        scope            = nameScope .&& isNotifyingScope
        nameScope        = SEL.any $ map (sWatchName .==) names
        isNotifyingScope = sWatchWState .== Notifying

mergeStaticWatches :: [NewWatch] -> WatchTable -> WatchTable
mergeStaticWatches watches table = foldl' mergeWatchIn table watches
  where mergeWatchIn table' staticW = case tryInsert of
                                        Just (x, table'') -> table''
                                        Nothing           -> forceUpdate
          where forceUpdate = SS.update' (Just . mergeWatch)
                                         (sWatchName .== (staticW ^. watchName))
                                         table'
                mergeWatch eWatch = eWatch & watchInterval      .~ (staticW ^. watchInterval)
                                           & watchNotifications .~ (staticW ^. watchNotifications)
                tryInsert = SS.insert staticW table'

emptyTable :: WatchTable
emptyTable = S.empty

fromList :: [NewWatch] -> WatchTable
fromList = SS.fromList' --foldl' (\table w -> snd $ createWatch w table) emptyTable

-- ACID State
allWatchesEvent :: Query AppState [EWatch]
allWatchesEvent = view (wTable . to allWatches)

createWatchEvent :: NewWatch -> Update AppState EWatch
createWatchEvent w = wTable %%= (createWatch w)

deleteWatchEvent :: WatchName -> Update AppState ()
deleteWatchEvent i = wTable %= (deleteWatch i)

findWatchEvent :: WatchName -> Query AppState (Maybe EWatch)
findWatchEvent i = view (wTable . findWatch')
  where findWatch' = to $ findWatch i

checkInWatchEvent :: POSIXTime -> WatchName -> Update AppState ()
checkInWatchEvent t i = wTable %= (checkInWatch t i)

pauseWatchEvent :: WatchName -> Update AppState ()
pauseWatchEvent i = wTable %= (pauseWatch i)

unPauseWatchEvent :: POSIXTime -> WatchName -> Update AppState ()
unPauseWatchEvent t i = wTable %= (unPauseWatch t i)

sweepTableEvent :: POSIXTime -> Update AppState ()
sweepTableEvent t = wTable %= (sweepTable t)

getNotifyingEvent :: Query AppState [EWatch]
getNotifyingEvent = view (wTable . to getNotifying)

completeNotifyingEvent :: [WatchName] -> Update AppState ()
completeNotifyingEvent is = wTable %= (completeNotifying is)

mergeStaticWatchesEvent :: [NewWatch] -> Update AppState ()
mergeStaticWatchesEvent watches = wTable %= (mergeStaticWatches watches)

$(makeAcidic ''AppState [ 'allWatchesEvent
                        , 'createWatchEvent
                        , 'deleteWatchEvent
                        , 'findWatchEvent
                        , 'checkInWatchEvent
                        , 'pauseWatchEvent
                        , 'unPauseWatchEvent
                        , 'sweepTableEvent
                        , 'getNotifyingEvent
                        , 'completeNotifyingEvent
                        , 'mergeStaticWatchesEvent ])

allWatchesS :: (QueryEvent AllWatchesEvent, MonadIO m)
               => AcidState (EventState AllWatchesEvent)
               -> m [EWatch]
allWatchesS acid = query' acid AllWatchesEvent

createWatchS :: (UpdateEvent CreateWatchEvent, MonadIO m)
                => AcidState (EventState CreateWatchEvent)
                -> NewWatch
                -> m EWatch
createWatchS acid = update' acid . CreateWatchEvent

deleteWatchS :: (UpdateEvent DeleteWatchEvent, MonadIO m)
                => AcidState (EventState DeleteWatchEvent)
                -> WatchName
                -> m ()
deleteWatchS acid = update' acid . DeleteWatchEvent

findWatchS :: (QueryEvent FindWatchEvent, MonadIO m)
              => AcidState (EventState FindWatchEvent)
              -> WatchName
              -> m (Maybe EWatch)
findWatchS acid = query' acid . FindWatchEvent

checkInWatchS :: (UpdateEvent CheckInWatchEvent, MonadIO m)
                => AcidState (EventState CheckInWatchEvent)
                -> POSIXTime
                -> WatchName
                -> m ()
checkInWatchS acid t = update' acid . CheckInWatchEvent t

pauseWatchS :: (UpdateEvent PauseWatchEvent, MonadIO m)
                => AcidState (EventState PauseWatchEvent)
                -> WatchName
                -> m ()
pauseWatchS acid = update' acid . PauseWatchEvent

unPauseWatchS :: (UpdateEvent UnPauseWatchEvent, MonadIO m)
                => AcidState (EventState UnPauseWatchEvent)
                -> POSIXTime
                -> WatchName
                -> m ()
unPauseWatchS acid t = update' acid . UnPauseWatchEvent t

sweepTableS :: (UpdateEvent SweepTableEvent, MonadIO m)
                => AcidState (EventState SweepTableEvent)
                -> POSIXTime
                -> m ()
sweepTableS acid = update' acid . SweepTableEvent

getNotifyingS :: (QueryEvent GetNotifyingEvent, MonadIO m)
              => AcidState (EventState GetNotifyingEvent)
              -> m [EWatch]
getNotifyingS acid = query' acid $ GetNotifyingEvent

completeNotifyingS :: (UpdateEvent CompleteNotifyingEvent, MonadIO m)
                   => AcidState (EventState CompleteNotifyingEvent)
                   -> [WatchName]
                   -> m ()
completeNotifyingS acid = update' acid . CompleteNotifyingEvent

mergeStaticWatchesS :: (UpdateEvent CompleteNotifyingEvent, MonadIO m)
                    => AcidState (EventState MergeStaticWatchesEvent)
                    -> [NewWatch]
                    -> m ()
mergeStaticWatchesS acid = update' acid . MergeStaticWatchesEvent
