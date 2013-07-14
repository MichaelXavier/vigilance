{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Utils.Vigilance.Types where

import Control.Applicative ((<$>))
import Control.Lens hiding ((.=))
import Control.Lens.TH
import Data.Monoid
import Data.Table
import Data.Time (UTCTime)
import Data.Text (Text)

newtype ID = ID { _unID :: Int } deriving (Show, Eq, Ord, Num)

makeClassy ''ID

data WatchInterval = Every Integer TimeUnit deriving (Show, Eq)

data TimeUnit = Seconds |
                Minutes |
                Hours   |
                Days    |
                Weeks   |
                Years deriving (Show, Eq)

data WatchState = Active    |
                  Paused    |
                  Notifying |
                  Triggered deriving (Show, Eq)

instance Monoid WatchState where
  mempty                = Paused
  mappend Paused Paused = Paused
  mappend x Paused      = x
  mappend Paused y      = y
  mappend x y           = y

newtype EmailAddress = EmailAddress { _unEmailAddress :: Text } deriving (Show, Eq)

makeClassy ''EmailAddress

data NotificationPreference = EmailNotification EmailAddress deriving (Show, Eq)

data WatchReport = WatchReport { _wrState       :: WatchState
                               , _wrLastCheckin :: Maybe UTCTime } deriving (Show, Eq)

makeClassy ''WatchReport

--TODO: notification backend
data Watch i = Watch { _watchId            :: i
                     , _watchName          :: Text
                     , _watchInterval      :: WatchInterval
                     , _watchWReport       :: WatchReport
                     , _watchNotifications :: [NotificationPreference] } deriving (Show, Eq)

makeLenses ''Watch

type NewWatch = Watch ()
type EWatch   = Watch ID

type WatchTable = Table EWatch

instance Tabular EWatch where
  type PKT EWatch = ID
  data Key k EWatch b where
    WatchID :: Key Primary EWatch ID
  data Tab EWatch i = WatchTable (i Primary ID)

  fetch WatchID = _watchId

  primary             = WatchID
  primarily WatchID r = r

  mkTab f = WatchTable <$> f WatchID

  forTab (WatchTable x) f      = WatchTable <$> f WatchID x
  ixTab (WatchTable x) WatchID = x

  autoTab = autoIncrement watchId
