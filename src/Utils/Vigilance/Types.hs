module Utils.Vigilance.Types ( Watch(..)
                             , NewWatch(..)
                             , EWatch(..)) where

type ID = Text

--TODO: notification backend
data Watch i = Watch { watchId           :: i
                     , watchName         :: Text
                     , watchInterval     :: WatchInterval
                     , watchNotification :: NotificationPreference } deriving (Show, Eq)

type NewWatch = Watch ()
type EWatch = Watch ID
