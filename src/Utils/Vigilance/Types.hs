{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Utils.Vigilance.Types where

import Control.Applicative ( (<$>)
                           , (<*>)
                           , pure)
import Control.Lens hiding ((.=))
import Control.Lens.TH
import Data.Aeson
import qualified Data.Attoparsec.Number as N
import Data.Monoid
import Data.SafeCopy ( base
                     , SafeCopy
                     , deriveSafeCopy)
import Data.Table
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Text (Text)
import Data.Typeable (Typeable)
import qualified Data.Vector as V

newtype ID = ID { _unID :: Int } deriving ( Show
                                          , Eq
                                          , Ord
                                          , Num
                                          , SafeCopy
                                          , FromJSON
                                          , ToJSON
                                          , Typeable)

makeClassy ''ID

data WatchInterval = Every Integer TimeUnit deriving (Show, Eq)

instance ToJSON WatchInterval where
  toJSON (Every n u) = Array $ V.fromList [toJSON n, toJSON u]
  
instance FromJSON WatchInterval where
  parseJSON = withArray "WatchInterval" $ parseWatchInterval . V.toList
    where parseWatchInterval [n@(Number (N.I _)), s@(String _)] = Every <$> parseJSON n <*> parseJSON s
          parseWatchInterval _                              = fail "expecting a pair of integer and string"

data TimeUnit = Seconds |
                Minutes |
                Hours   |
                Days    |
                Weeks   |
                Years deriving (Show, Eq)

instance ToJSON TimeUnit where
  toJSON Seconds = String "seconds"
  toJSON Minutes = String "minutes"
  toJSON Hours   = String "hours"
  toJSON Days    = String "days"
  toJSON Weeks   = String "weeks"
  toJSON Years   = String "years"

instance FromJSON TimeUnit where
  parseJSON = withText "TimeUnit" parseTimeUnit
    where parseTimeUnit "seconds" = pure Seconds
          parseTimeUnit "minutes" = pure Minutes
          parseTimeUnit "hours"   = pure Hours
          parseTimeUnit "days"    = pure Days
          parseTimeUnit "weeks"   = pure Weeks
          parseTimeUnit "years"   = pure Years
          parseTimeUnit _         = fail "Unknown time unit"

data WatchState = Active    |
                  Paused    |
                  Notifying |
                  Triggered deriving (Show, Eq)

instance Monoid WatchState where
  mempty                = Paused
  mappend Paused Paused = Paused
  mappend x      Paused = x
  mappend _      y      = y

instance ToJSON WatchState where
  toJSON Active    = String "active"
  toJSON Paused    = String "paused"
  toJSON Notifying = String "notifying"
  toJSON Triggered = String "triggered"

instance FromJSON WatchState where
  parseJSON = withText "WatchState" parseWatchState
    where parseWatchState "active"    = pure Active
          parseWatchState "paused"    = pure Paused
          parseWatchState "notifying" = pure Notifying
          parseWatchState "triggered" = pure Triggered
          parseWatchState _           = fail "Invalid value"

newtype EmailAddress = EmailAddress { _unEmailAddress :: Text } deriving ( Show
                                                                         , Eq
                                                                         , SafeCopy
                                                                         , Typeable
                                                                         , ToJSON
                                                                         , FromJSON)

makeClassy ''EmailAddress

data NotificationPreference = EmailNotification EmailAddress deriving (Show, Eq)

instance ToJSON NotificationPreference where
  toJSON (EmailNotification a) = object [ "type"    .= String "email"
                                        , "address" .= String (a ^. unEmailAddress)]

instance FromJSON NotificationPreference where
  parseJSON = withObject "EmailNotification" parseEmail --TODO: more
    where parseEmail obj = EmailNotification <$> obj .: "address" --TODO: NOT CORRECt

data WatchReport = WatchReport { _wrState       :: WatchState
                               , _wrLastCheckin :: Maybe POSIXTime } deriving (Show, Eq)

makeClassy ''WatchReport

instance ToJSON WatchReport where
  toJSON wr = object [ "state"        .= (wr ^. wrState)
                     , "last_checkin" .= (POSIXWrapper <$> wr ^. wrLastCheckin)]

instance FromJSON WatchReport where
  parseJSON = withObject "WatchReport" parseWatchReport
    where parseWatchReport obj = WatchReport <$> obj .:? "state" .!= mempty
                                             <*> (unwrapTime <$> obj .:? "last_checkin")
          unwrapTime = fmap unPOSIXWrapper

newtype POSIXWrapper = POSIXWrapper { unPOSIXWrapper :: POSIXTime }

instance FromJSON POSIXWrapper where
  parseJSON = withNumber "POSIXTime" parsePOSIXTime
    where parsePOSIXTime (N.I i) = pure . POSIXWrapper . fromIntegral $ i
          parsePOSIXTime _       = fail "Expected integer"

instance ToJSON POSIXWrapper where
  toJSON = Number . N.I . truncate . toRational . unPOSIXWrapper


--TODO: notification backend
data Watch i = Watch { _watchId            :: i
                     , _watchName          :: Text
                     , _watchInterval      :: WatchInterval
                     , _watchWReport       :: WatchReport
                     , _watchNotifications :: [NotificationPreference] } deriving (Show, Eq, Typeable)

makeLenses ''Watch

type NewWatch = Watch ()
type EWatch   = Watch ID

instance ToJSON a => ToJSON (Watch a) where
  toJSON w = object [ "id"            .= (w ^. watchId)
                    , "name"          .= (w ^. watchName)
                    , "interval"      .= (w ^. watchInterval)
                    , "report"        .= (w ^. watchWReport)
                    , "notifications" .= (w ^. watchNotifications)
                    , "name"          .= (w ^. watchName) ]

instance FromJSON a => FromJSON (Watch a) where
  parseJSON = withObject "Watch" parseNewWatch
    where parseNewWatch obj = Watch <$> obj .: "id"
                                    <*> obj .: "name"
                                    <*> obj .: "interval"
                                    <*> obj .: "report"
                                    <*> obj .: "notifications"

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

data AppState = AppState { _wTable :: WatchTable } deriving (Typeable)

makeLenses ''AppState

deriveSafeCopy 0 'base ''WatchState
deriveSafeCopy 0 'base ''TimeUnit
deriveSafeCopy 0 'base ''WatchInterval
deriveSafeCopy 0 'base ''WatchReport
deriveSafeCopy 0 'base ''Watch
deriveSafeCopy 0 'base ''NotificationPreference
deriveSafeCopy 0 'base ''AppState
