{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module Utils.Vigilance.Types ( Watch(..)
                             , HasWatch(..)
                             , ID(..)
                             , HasID(..)
                             , EmailAddress(..)
                             , HasEmailAddress(..)
                             , NewWatch(..)
                             , NotificationPreference(..)
                             , WatchInterval(..)
                             , WatchState(..)
                             , TimeUnit(..)
                             , EWatch(..)) where

import Control.Lens hiding ((.=))
import Control.Lens.TH
import Data.Monoid
import Data.Time (UTCTime)
import Data.Text (Text)

newtype ID = ID { _unID :: Text } deriving (Show, Eq)

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

makeClassy ''Watch

type NewWatch = Watch ()
type EWatch   = Watch ID
