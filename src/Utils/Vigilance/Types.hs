{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
module Utils.Vigilance.Types where

import Prelude (FilePath)
import ClassyPrelude hiding (FilePath)
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.SafeCopy (base, deriveSafeCopy)
import           Data.Store ( M
                            , O
                            , (:.)
                            , Store )
import qualified Data.Store as S
import           Data.Store.Storable (Storable(..))
import Data.String.Conversions (cs)
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Vector as V
import Network.Http.Client ( URL
                           , StatusCode )
import System.Log.FastLogger ( ToLogStr(..) )
import Text.InterpolatedString.Perl6 (qc)
import Yesod.Core.Dispatch (PathPiece)

newtype ID = ID { _unID :: Int } deriving ( Show
                                          , Eq
                                          , Enum
                                          , Read
                                          , PathPiece
                                          , Ord
                                          , Num
                                          , FromJSON
                                          , ToJSON
                                          , Typeable)

deriveSafeCopy 0 'base ''ID
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
    where parseWatchInterval [Number n, s@(String _)]
            | n > 0     = Every <$> pure (round n) <*> parseJSON s
            | otherwise = fail "interval must be > 0"
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

txtToTimeUnit :: Text -> Maybe TimeUnit
txtToTimeUnit "seconds" = Just Seconds
txtToTimeUnit "minutes" = Just Minutes
txtToTimeUnit "hours"   = Just Hours
txtToTimeUnit "days"    = Just Days
txtToTimeUnit "weeks"   = Just Weeks
txtToTimeUnit "years"   = Just Years
txtToTimeUnit _         = Nothing

instance FromJSON TimeUnit where
  parseJSON = withText "TimeUnit" parseTimeUnit
    where parseTimeUnit txt = maybe unknown return $ txtToTimeUnit txt
          unknown           = fail "unknown time unit"

newtype EmailAddress = EmailAddress { _unEmailAddress :: Text } deriving ( Show
                                                                         , Eq
                                                                         , Ord
                                                                         , Typeable
                                                                         , ToJSON
                                                                         , FromJSON)

deriveSafeCopy 0 'base ''EmailAddress
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
          parseHttp obj = do
              String "http" <- obj .: "type"
              String url    <- obj .: "url"
              pure . HTTPNotification $ cs url

newtype POSIXWrapper = POSIXWrapper { unPOSIXWrapper :: POSIXTime }

instance FromJSON POSIXWrapper where
  parseJSON = withScientific "POSIXTime" parsePOSIXTime
    where parsePOSIXTime = pure . POSIXWrapper . fromRational . toRational

instance ToJSON POSIXWrapper where
  toJSON = Number . fromRational . toRational . unPOSIXWrapper

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
instance Data.Store.Storable.Storable NewWatch where
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

data NotificationError = FailedByCode StatusCode |
                         FailedByException Text deriving (Eq, Show, Typeable)

instance ToJSON NotificationError where
  toJSON (FailedByCode c) = object [ "error"       .= String "code_error"
                                   , "status_code" .= c
                                   , "message"     .= String [qc|Failed with status code {c}|] ]
  toJSON (FailedByException e) = object [ "error"     .= String "exception"
                                        , "exception" .= e
                                        , "message"   .= String [qc|Failed with exception {e}|] ]

instance FromJSON NotificationError where
  parseJSON = withObject "NotificationError" parseNotificationError
    where parseNotificationError obj = do
            err <- obj .: "error"
            case err of
              String "code_error" -> FailedByCode      <$> obj .: "status_code"
              String "exception"  -> FailedByException <$> obj .: "exception"
              _                   -> mzero

data FailedNotification = FailedNotification { _failedWatch     :: EWatch
                                             , _failedPref      :: NotificationPreference
                                             , _failedLastError :: NotificationError
                                             , _retries         :: Int } deriving (Typeable, Show, Eq)

instance ToJSON FailedNotification where
  toJSON FailedNotification{..} = object [ "failed_watch"        .= _failedWatch
                                         , "failed_notification" .= _failedPref
                                         , "last_error"          .= _failedLastError
                                         , "retries"             .= _retries ]

instance FromJSON FailedNotification where
  parseJSON = withObject "FailedNotification" parseFailedNotification
    where parseFailedNotification obj = FailedNotification <$> obj .: "failed_watch"
                                                           <*> obj .: "failed_notification"
                                                           <*> obj .: "last_error"
                                                           <*> obj .: "retries"

makeClassy ''FailedNotification

data AppState = AppState { _wTable :: WatchTable
                         , _failed :: [FailedNotification] } deriving (Typeable)

makeLenses ''AppState

data LogCfg = LogCfg { _logCfgPath    :: FilePath
                     , _logCfgVerbose :: Bool } deriving (Show, Eq)

makeClassy ''LogCfg

data Config = Config { _configAcidPath   :: FilePath
                     , _configFromEmail  :: Maybe EmailAddress
                     , _configPort       :: Int
                     , _configLogCfg     :: LogCfg
                     , _configWatches    :: [NewWatch]
                     , _configMaxRetries :: Int } deriving (Show, Eq)

makeClassy ''Config

-- this is unsound
instance Monoid Config where
  mempty = Config defaultAcidPath Nothing defaultPort defaultLogCfg mempty defaultMaxRetries
  Config apa ea pa la wa ra `mappend` Config apb eb pb lb wb rb = Config (nonDefault defaultAcidPath apa apb)
                                                                         (chooseJust ea eb)
                                                                         (nonDefault defaultPort pa pb)
                                                                         (nonDefault defaultLogCfg la lb)
                                                                         (mappend wa wb)
                                                                         (nonDefault defaultMaxRetries ra rb)
    where chooseJust a@(Just _) _ = a
          chooseJust _ b          = b
          nonDefault defValue a b
            | a == defValue = b
            | b == defValue = a
            | otherwise     = b

defaultMaxRetries :: Int
defaultMaxRetries = 3

defaultLogCfg :: LogCfg
defaultLogCfg = LogCfg defaultLogPath False

defaultLogPath :: FilePath
defaultLogPath = vigilanceDir <> "/vigilance.log"

defaultAcidPath :: FilePath
defaultAcidPath = vigilanceDir <> "/state/AppState"

defaultPort :: Int
defaultPort = 3000

vigilanceDir :: FilePath
vigilanceDir = "$(HOME)/.vigilance"

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

newtype EmailNotifier = EmailNotifier { _emailNotifier :: Notifier }

makeFields ''EmailNotifier

newtype HTTPNotifier = HTTPNotifier { _httpNotifier :: Notifier }

makeFields ''HTTPNotifier

newtype LogNotifier = LogNotifier { _logNotifier  :: Notifier }

makeFields ''LogNotifier

data NotifierGroup = NotifierGroup { _ngEmail :: Maybe EmailNotifier
                                   , _ngHTTP  :: HTTPNotifier
                                   , _ngLog   :: LogNotifier }

makeClassy ''NotifierGroup

deriveSafeCopy 0 'base ''WatchName
deriveSafeCopy 0 'base ''WatchState
deriveSafeCopy 0 'base ''TimeUnit
deriveSafeCopy 0 'base ''WatchInterval
deriveSafeCopy 0 'base ''Watch
deriveSafeCopy 0 'base ''NotificationPreference
deriveSafeCopy 0 'base ''NotificationError
deriveSafeCopy 0 'base ''FailedNotification
deriveSafeCopy 0 'base ''AppState
