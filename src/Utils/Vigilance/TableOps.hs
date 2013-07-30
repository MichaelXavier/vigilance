{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
module Utils.Vigilance.TableOps ( createWatch
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
                                , fromList
                                , emptyTable) where

import Control.Lens
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ask)
import Control.Monad.State ( get
                           , put)
import Data.Acid
import Data.Acid.Advanced (update', query')
import Data.List (foldl')
import Data.Table ( insert'
                  , with
                  , rows'
                  , empty
                  , deleteWith)
import qualified Data.Table as T
import Data.Time.Clock.POSIX (POSIXTime)
import Utils.Vigilance.Sweeper (sweepWatch)
import Utils.Vigilance.Types

createWatch :: NewWatch -> WatchTable -> (EWatch, WatchTable)
createWatch w = insert' $ w & watchId .~ (ID 0)

deleteWatch :: ID -> WatchTable -> WatchTable
deleteWatch i table = table & deleteWith WatchID (==) i

findWatch :: ID -> WatchTable -> Maybe EWatch
findWatch i table = table ^. at i

watchLens :: (Indexable ID p0, Profunctor p0)
             => p0 EWatch EWatch
             -> ID
             -> WatchTable
             -> WatchTable
watchLens f i table = table & ix i %~ f

checkInWatch :: POSIXTime -> ID -> WatchTable -> WatchTable
checkInWatch time = watchLens doCheckIn
  where doCheckIn w        = w & watchWState %~ updateState
        updateState Paused = Paused
        updateState _      = Active time

pauseWatch :: ID -> WatchTable -> WatchTable
pauseWatch = watchLens pause
  where pause w = w & watchWState .~ Paused

unPauseWatch :: POSIXTime -> ID -> WatchTable -> WatchTable
unPauseWatch t = watchLens unPause
  where unPause w = w & watchWState %~ updateState
        updateState (Active newTime) = Active newTime
        updateState _                = Active t

sweepTable :: POSIXTime -> WatchTable -> WatchTable
sweepTable time table = table & rows' %~ sweep
  where sweep = sweepWatch time

getNotifying :: WatchTable -> [EWatch]
getNotifying table = table ^.. with WatchWState (==) Notifying . rows'

--TODO: also scope by state
-- hack, see https://github.com/ekmett/tables/issues/6
-- so not performant
completeNotifying :: [ID] -> WatchTable -> WatchTable
completeNotifying ids table = foldl' updateOne table ids
--completeNotifying ids table = table & with WatchWState (==) Notifying . T.withAny WatchID ids . T.rows' %~ updateState
--completeNotifying ids table = table & with WatchWState (==) Notifying . T.withAny (toListOf folded . T.fetch WatchID) ids . rows' %~ updateState
--completeNotifying ids table = table & T.withAny (toListOf folded . T.fetch WatchID) ids . rows' %~ updateState
  where updateState :: EWatch -> EWatch
        updateState w = w & watchWState .~ Triggered
        updateOne :: WatchTable -> ID -> WatchTable
        updateOne = flip $ watchLens updateState

emptyTable :: WatchTable
emptyTable = empty

fromList :: [NewWatch] -> WatchTable
fromList = foldl' (\table w -> snd $ createWatch w table) emptyTable

-- ACID State
-- this compiles and is mega slick but needs testing
createWatchEvent :: NewWatch -> Update AppState EWatch
createWatchEvent w = wTable %%= (createWatch w)

deleteWatchEvent :: ID -> Update AppState ()
deleteWatchEvent i = wTable %= (deleteWatch i)

findWatchEvent :: ID -> Query AppState (Maybe EWatch)
findWatchEvent i = view (wTable . findWatch')
  where findWatch' = to $ findWatch i

checkInWatchEvent :: POSIXTime -> ID -> Update AppState ()
checkInWatchEvent t i = wTable %= (checkInWatch t i)

pauseWatchEvent :: ID -> Update AppState ()
pauseWatchEvent i = wTable %= (pauseWatch i)

unPauseWatchEvent :: POSIXTime -> ID -> Update AppState ()
unPauseWatchEvent t i = wTable %= (unPauseWatch t i)

sweepTableEvent :: POSIXTime -> Update AppState ()
sweepTableEvent t = wTable %= (sweepTable t)

getNotifyingEvent :: Query AppState [EWatch]
getNotifyingEvent = view (wTable . to getNotifying)

$(makeAcidic ''AppState [ 'createWatchEvent
                        , 'deleteWatchEvent
                        , 'findWatchEvent
                        , 'checkInWatchEvent
                        , 'pauseWatchEvent
                        , 'unPauseWatchEvent
                        , 'sweepTableEvent
                        , 'getNotifyingEvent])

createWatchS :: (UpdateEvent CreateWatchEvent, MonadIO m)
                => AcidState (EventState CreateWatchEvent)
                -> NewWatch
                -> m EWatch
createWatchS acid = update' acid . CreateWatchEvent

deleteWatchS :: (UpdateEvent DeleteWatchEvent, MonadIO m)
                => AcidState (EventState DeleteWatchEvent)
                -> ID
                -> m ()
deleteWatchS acid = update' acid . DeleteWatchEvent

findWatchS :: (QueryEvent FindWatchEvent, MonadIO m)
              => AcidState (EventState FindWatchEvent)
              -> ID
              -> m (Maybe EWatch)
findWatchS acid = query' acid . FindWatchEvent

checkInWatchS :: (UpdateEvent CheckInWatchEvent, MonadIO m)
                => AcidState (EventState CheckInWatchEvent)
                -> POSIXTime
                -> ID
                -> m ()
checkInWatchS acid t = update' acid . CheckInWatchEvent t

pauseWatchS :: (UpdateEvent PauseWatchEvent, MonadIO m)
                => AcidState (EventState PauseWatchEvent)
                -> ID
                -> m ()
pauseWatchS acid = update' acid . PauseWatchEvent

unPauseWatchS :: (UpdateEvent UnPauseWatchEvent, MonadIO m)
                => AcidState (EventState UnPauseWatchEvent)
                -> POSIXTime
                -> ID
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
