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

$(makeAcidic ''AppState [ 'createWatchEvent
                        , 'deleteWatchEvent
                        , 'findWatchEvent
                        , 'checkInWatchEvent
                        , 'pauseWatchEvent
                        , 'unPauseWatchEvent
                        , 'sweepTableEvent])

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
