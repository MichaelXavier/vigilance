{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils.Vigilance.Config ( configNotifiers
                              , convertConfig
                              , reloadConfig
                              , loadRawConfig
                              , loadConfig) where

import ClassyPrelude hiding (FilePath)
import Control.Applicative ( (<$>)
                           , (<*>) )
import Control.Monad -- ((<=<))
import Control.Monad.Reader (ask)
import Control.Lens
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import GHC.IO (FilePath)
import qualified Utils.Vigilance.Notifiers.Email as E
import qualified Utils.Vigilance.Notifiers.Log   as L
import Utils.Vigilance.Types

configNotifiers :: Config -> LogCtx IO [Notifier]
configNotifiers cfg = do logNotifier        <- L.notify <$> ask
                         let mEmailNotifier = E.notify . E.EmailContext <$> cfg ^. configFromEmail
                         return $ catMaybes [Just logNotifier, mEmailNotifier]

loadRawConfig :: FilePath -> IO CT.Config
loadRawConfig = C.load . return . CT.Required

loadConfig :: FilePath -> IO Config
loadConfig = convertConfig <=< loadRawConfig

-- basically no point to this mappend at present
convertConfig :: CT.Config -> IO Config
convertConfig cfg = mempty <> Config <$> lud defaultAcidPath "vigilance.acid_path"
                                     <*> (toEmailAddress <$> lu "vigilance.from_email")
                                     <*> (lud defaultPort "vigilance.port")
                                     <*> lud defaultLogPath "vigilance.log_path"
                                     <*> (parseWatches <$> C.getMap cfg)
  where lu             = C.lookup cfg
        lud d          = C.lookupDefault d cfg
        toEmailAddress = fmap (EmailAddress . pack)

reloadConfig :: CT.Config -> IO ()
reloadConfig = C.reload

-- probably want to make this an either to fail on parse failures
parseWatches :: HashMap CT.Name CT.Value -> [NewWatch]
parseWatches globalCfg = HM.foldrWithKey addWatch [] rawWatches -- probably use a traverse
  where rawWatches :: HashMap Text WatchAttrs
        rawWatches = HM.foldrWithKey appendGroup mempty globalCfg

type WatchAttrs = HashMap CT.Name CT.Value

appendGroup :: CT.Name -> CT.Value -> HashMap Text WatchAttrs -> HashMap Text WatchAttrs
appendGroup fullKey val acc
  | nnull wName && nnull wAttr = HM.insertWith mappend wName (HM.singleton wAttr val) acc
  | otherwise                  = acc
  where (_, localKey)                = T.breakOnEnd "vigilance.watches." fullKey
        (wName, wAttrWithLeadingDot) = T.breakOn "." localKey
        wAttr                        = T.drop 1 wAttrWithLeadingDot
        nnull = not . null

addWatch :: Text -> WatchAttrs -> [NewWatch] -> [NewWatch]
addWatch wName attrs = mappend watches
  where watches = maybeToList $ buildWatch wName attrs

buildWatch :: Text -> WatchAttrs -> Maybe NewWatch
buildWatch wName attrs = Watch <$> pure ()
                               <*> pure wName
                               <*> (parseInterval =<< lu "interval")
                               <*> pure mempty
                               <*> (pure . parseNotifications $ lud noNotifications "notifications") -- ehh, list of lists not that great, but making arbitrary names for notifications is dumb too
  where lu k            = HM.lookup k attrs
        lud d k         = HM.lookupDefault d k attrs
        noNotifications = CT.List []

parseNotifications :: CT.Value -> [NotificationPreference]
parseNotifications (CT.List vs) = catMaybes $ map parseNotification vs

parseNotification :: CT.Value -> Maybe NotificationPreference
parseNotification (CT.List [CT.String "email", CT.String a]) = Just . EmailNotification . EmailAddress $ a
parseNotification _                                          = Nothing

parseInterval :: CT.Value -> Maybe WatchInterval
parseInterval (CT.List [CT.Number n, CT.String unit]) = Every <$> (pure . truncate $ n) 
                                                              <*> txtToInterval unit
  where txtToInterval "seconds" = Just Seconds -- duplicates Types FromJSON TimeUnit
        txtToInterval "minutes" = Just Minutes
        txtToInterval "hours"   = Just Hours
        txtToInterval "days"    = Just Days
        txtToInterval "weeks"   = Just Weeks
        txtToInterval "years"   = Just Years
        txtToInterval _         = Nothing
parseInterval _                                      = Nothing
