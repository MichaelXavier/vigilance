{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Utils.Vigilance.Types where

import Prelude (FilePath)
import ClassyPrelude hiding (FilePath)
import Control.Concurrent.STM.TChan (TChan)
import Control.Monad (mzero)
import Control.Monad.Reader (ReaderT)
import Control.Lens hiding ((.=))
import Data.Aeson
import qualified Data.Attoparsec.Number as N
import Data.SafeCopy ( base
                     , SafeCopy
                     , deriveSafeCopy)
import           Data.Store ( M
                            , O
                            , (:.)
                            , Store )
import qualified Data.Store as S
import           Data.Store.Storable (Storable(..))
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Vector as V
import Network.Http.Client ( URL
                           , StatusCode )
import Numeric.Natural (Natural)
import System.Log.FastLogger ( ToLogStr(..) )
import Yesod.Core.Dispatch (PathPiece)

newtype ID = ID { _unID :: Int } deriving ( Show
                                          , Eq
                                          , Enum
                                          , Read --testme, this is unlikely to go well. we don't want the quotes
                                          , PathPiece
                                          , Ord
                                          , Num
                                          , SafeCopy
                                          , FromJSON
                                          , ToJSON
                                          , Typeable)

makeClassy ''ID

instance Bounded ID where
  minBound = ID 1
  maxBound = ID maxBound

--TODO: somewhere have to handle the case of <= 0
data WatchInterval = Every Integer TimeUnit deriving (Show, Eq, Typeable, Ord)

instance ToJSON WatchInterval where
  toJSON (Every n u) = Array $ V.fromList [toJSON n, toJSON u]
  
instance FromJSON WatchInterval where
  parseJSON = withArray "WatchInterval" $ parseWatchInterval . V.toList
    where parseWatchInterval [Number (N.I n), s@(String _)] = Every <$> pure n <*> parseJSON s -- just get it out of the N.I and call pure?
          parseWatchInterval _                              = fail "expecting a pair of integer and string"

data TimeUnit = Seconds |
                Minutes |
                Hours   |
                Days    |
                Weeks   |
                Years deriving (Show, Eq, Ord)

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

newtype EmailAddress = EmailAddress { _unEmailAddress :: Text } deriving ( Show
                                                                         , Eq
                                                                         , Ord
                                                                         , SafeCopy
                                                                         , Typeable
                                                                         , ToJSON
                                                                         , FromJSON)

makeClassy ''EmailAddress

data NotificationPreference = EmailNotification EmailAddress |
                              HTTPNotification URL deriving (Show, Eq, Ord)

instance ToJSON NotificationPreference where
  toJSON (EmailNotification a) = object [ "type"    .= String "email"
                                        , "address" .= String (a ^. unEmailAddress)]
  toJSON (HTTPNotification u) = object [ "type"    .= String "http"
                                       , "url"     .= String (decodeUtf8 u)]

instance FromJSON NotificationPreference where
  parseJSON v = parseEmailNotification v <|>
                parseHTTPNotification v
    where parseEmailNotification = withObject "EmailNotification" parseEmail
          parseEmail obj = do t <- obj .: "type"
                              case t of
                                String "email" -> EmailNotification <$> obj .: "address"
                                _              -> mzero
          parseHTTPNotification = withObject "HTTPNotification" parseHttp
          parseHttp obj = do t <- obj .: "type"
                             case t of
                               String "http" -> HTTPNotification <$> obj .: "url"
                               _             -> mzero

newtype POSIXWrapper = POSIXWrapper { unPOSIXWrapper :: POSIXTime }

instance FromJSON POSIXWrapper where
  parseJSON = withNumber "POSIXTime" parsePOSIXTime
    where parsePOSIXTime (N.I i) = pure . POSIXWrapper . fromIntegral $ i
          parsePOSIXTime _       = fail "Expected integer"

instance ToJSON POSIXWrapper where
  toJSON = Number . N.I . truncate . toRational . unPOSIXWrapper

data WatchState = Active { _lastCheckIn :: POSIXTime } |
                  Paused                               |
                  Notifying                            |
                  Triggered deriving (Show, Eq, Ord) -- ehhhhhh

makeClassy ''WatchState

instance Monoid WatchState where
  mempty                = Paused
  mappend Paused Paused = Paused
  mappend x      Paused = x
  mappend _      y      = y

instance ToJSON WatchState where
  toJSON (Active t) = object [ "name"          .= String "active"
                             , "last_check_in" .= POSIXWrapper t ]
  toJSON Paused     = object [ "name"          .= String "paused" ]
  toJSON Notifying  = object [ "name"          .= String "notifying" ]
  toJSON Triggered  = object [ "name"          .= String "triggered" ]

instance FromJSON WatchState where
  parseJSON = withObject "WatchState" parseWatchState
    where parseWatchState obj = withText "state name" (parseStateFromName obj) =<< (obj .: "name")
          parseStateFromName _ "paused"    = pure Paused
          parseStateFromName _ "notifying" = pure Notifying
          parseStateFromName _ "triggered" = pure Triggered
          parseStateFromName obj "active" = Active <$> (unPOSIXWrapper <$> obj .: "last_check_in")
          parseStateFromName _ _           = fail "Invalid value"


newtype WatchName = WatchName { _unWatchName :: Text } deriving ( Show
                                                                , Eq
                                                                , Ord
                                                                , FromJSON
                                                                , ToJSON
                                                                , IsString
                                                                , Read
                                                                , PathPiece ) -- not so sure about the isstring

makeLenses ''WatchName

data Watch i = Watch { _watchId            :: i
                     , _watchName          :: WatchName
                     , _watchInterval      :: WatchInterval
                     , _watchWState        :: WatchState
                     , _watchNotifications :: [NotificationPreference] } deriving (Show, Eq, Typeable, Ord)

makeLenses ''Watch

type NewWatch = Watch ()
type EWatch   = Watch ID

instance ToJSON EWatch where
  toJSON w = object [ "id"            .= (w ^. watchId)
                    , "name"          .= (w ^. watchName)
                    , "interval"      .= (w ^. watchInterval)
                    , "state"         .= (w ^. watchWState)
                    , "notifications" .= (w ^. watchNotifications)
                    , "name"          .= (w ^. watchName) ]


instance FromJSON EWatch where
  parseJSON = withObject "Watch" parseNewWatch
    where parseNewWatch obj = Watch <$> obj .: "id"
                                    <*> obj .: "name"
                                    <*> obj .: "interval"
                                    <*> obj .: "state"
                                    <*> obj .: "notifications"

instance ToJSON NewWatch where
  toJSON w = object [ "name"          .= (w ^. watchName)
                    , "interval"      .= (w ^. watchInterval)
                    , "state"         .= (w ^. watchWState)
                    , "notifications" .= (w ^. watchNotifications)
                    , "name"          .= (w ^. watchName) ]


instance FromJSON NewWatch where
  parseJSON = withObject "Watch" parseNewWatch
    where parseNewWatch obj = Watch <$> pure ()
                                    <*> obj .: "name"
                                    <*> obj .: "interval"
                                    <*> obj .: "state"
                                    <*> obj .: "notifications"

data WatchStoreTag = WatchStoreTag

-- tagspec
instance Storable NewWatch where
  type StoreTS   NewWatch = ID :. WatchName :. WatchInterval :. WatchState :. NotificationPreference
  type StoreKRS  NewWatch = O  :. O         :. O             :. O          :. M
  type StoreIRS  NewWatch = O  :. O         :. M             :. M          :. M

  key (Watch _ wn wi ws wns) = 
    S.dimA    S..:
    S.dimO wn S..:
    S.dimO wi S..:
    S.dimO ws S..:.
    S.dimM wns

type WatchTable = Store WatchStoreTag (StoreKRS NewWatch) (StoreIRS NewWatch) (StoreTS NewWatch) NewWatch

data FailedNotification = FailedNotification { _failedWatch     :: EWatch
                                             , _failedPref      :: NotificationPreference
                                             , _failedLastError :: SomeException
                                             , _retries         :: Natural }

makeClassy ''FailedNotification

data NotificationError = FailedByCode StatusCode deriving (Show, Eq, Typeable)

instance Exception NotificationError

newtype AppState = AppState { _wTable :: WatchTable } deriving (Typeable)

makeLenses ''AppState

data LogCfg = LogCfg { _logCfgPath    :: FilePath
                     , _logCfgVerbose :: Bool } deriving (Show, Eq)

makeClassy ''LogCfg

data Config = Config { _configAcidPath  :: FilePath
                     , _configFromEmail :: Maybe EmailAddress
                     , _configPort      :: Int
                     , _configLogCfg    :: LogCfg
                     , _configWatches   :: [NewWatch] } deriving (Show, Eq)

makeClassy ''Config

-- this is unsound
instance Monoid Config where
  mempty = Config defaultAcidPath Nothing defaultPort defaultLogCfg mempty
  Config apa ea pa la wa `mappend` Config apb eb pb lb wb = Config (nonDefault defaultAcidPath apa apb)
                                                                   (chooseJust ea eb)
                                                                   (nonDefault defaultPort pa pb)
                                                                   (nonDefault defaultLogCfg la lb)
                                                                   (mappend wa wb)
    where chooseJust a@(Just _) _ = a
          chooseJust _ b          = b
          nonDefault defValue a b
            | a == defValue = b
            | b == defValue = a
            | otherwise     = b

defaultLogCfg :: LogCfg
defaultLogCfg = LogCfg defaultLogPath False

defaultLogPath :: FilePath
defaultLogPath = "log/vigilance.log"

defaultAcidPath :: FilePath
defaultAcidPath = "state/AppState"

defaultPort :: Int
defaultPort = 3000

data LogMessage = LogMessage        Text |
                  VerboseLogMessage Text deriving (Show, Eq)

instance ToLogStr LogMessage where
  toLogStr (LogMessage x)        = toLogStr x
  toLogStr (VerboseLogMessage x) = toLogStr x

-- should i use chan, tmchan?
type LogChan = TChan [LogMessage]

-- maybe need a local ctx that can name the context and then nest a chan?
data LogCtx = LogCtx { _ctxName    :: Text
                     , _ctxChan    :: LogChan }

makeClassy ''LogCtx

type LogCtxT m a = ReaderT LogCtx m a

type Notifier = [EWatch] -> LogCtxT IO [FailedNotification]

deriveSafeCopy 0 'base ''WatchName
deriveSafeCopy 0 'base ''WatchState
deriveSafeCopy 0 'base ''TimeUnit
deriveSafeCopy 0 'base ''WatchInterval
deriveSafeCopy 0 'base ''Watch
deriveSafeCopy 0 'base ''NotificationPreference
deriveSafeCopy 0 'base ''AppState
