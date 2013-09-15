{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Utils.Vigilance.Notifiers.Email ( notify
                                       , generateEmails
                                       , EmailContext(..)
                                       , HasEmailContext(..)
                                       , NotificationMail(..)
                                       , HasNotificationMail(..)
                                       , Address(..)) where

import ClassyPrelude
import Control.Lens hiding (from)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Lazy as M
import Text.InterpolatedString.Perl6 (qc)
import Network.Mail.Mime ( Address(..)
                         , emptyMail
                         , Part(..)
                         , renderSendMail
                         , Encoding( QuotedPrintableText )
                         , Mail(..))

import Utils.Vigilance.Logger ( pushLog
                              , renameLogCtx )
import Utils.Vigilance.Types
import Utils.Vigilance.Utils (concatMapM)

data EmailContext = EmailContext { _fromEmail :: EmailAddress } deriving (Show, Eq)

makeClassy ''EmailContext

data NotificationMail = NotificationMail { _nmWatches :: [EWatch]
                                         , _nmMail    :: Mail }

makeClassy ''NotificationMail

notify :: EmailContext -> EmailNotifier
notify ctx = EmailNotifier notifierBody
  where notifierBody watches = renameLogCtx "Email Notifier" $ concatMapM renderSendMail' mails
          where mails = generateEmails watches ctx

renderSendMail' :: NotificationMail -> LogCtxT IO [FailedNotification]
renderSendMail' (NotificationMail ws mail) = do
  pushLog [qc|Sending email notification to {emailList}|]
  lift $ handleAny buildFailures $ renderSendMail mail >> return []
  where emailList = intercalate ", " emails
        emails    = map addressEmail . mailTo $ mail
        addrs     = map EmailAddress emails
        buildFailures e = return [ FailedNotification w n (FailedByException $ show e) 0
                                 | w <- ws
                                 , n@(EmailNotification addr) <- w ^. watchNotifications
                                 , addr `elem` addrs ]

generateEmails :: [EWatch] -> EmailContext -> [NotificationMail]
generateEmails watches ctx = M.elems $ M.mapWithKey createMail groupedByEmail -- ehhhhh
  where groupedByEmail = M.fromListWith mappend $ concatMap watchesWithEmails watches
        createMail toAddr ws = NotificationMail ws mail
          where mail = (emptyMail from) { mailTo      = [e2a toAddr]
                                        , mailHeaders = [("Subject", subject)]
                                        , mailParts   = [[mailPart ws]] }
                subject = [qc|Vigilence notification {watchCount} activated|] :: Text
                watchCount = length ws
        from :: Address
        from = ctx ^. fromEmail . to e2a

mailPart :: [EWatch] -> Part
mailPart ws =  Part "text/plain; charset=utf-8" QuotedPrintableText Nothing [] (bodyLBS ws)

bodyLBS :: [EWatch] -> LBS.ByteString
bodyLBS ws = [qc|
The following watches were triggered:

{watchSummary}

Sincerely,
Vigilence
  |]
  where watchSummary = mconcat $ map summarize ws
        summarize w  = [qc|- {name} ({interval})|] :: LBS.ByteString
          where interval = w ^. watchInterval
                name     = w ^. watchName . unWatchName . to unpack

watchesWithEmails :: EWatch -> [(EmailAddress, [EWatch])]
watchesWithEmails w = zip emails (repeat [w] :: [[EWatch]])
  where emails = mapMaybe extractEmail $ w ^. watchNotifications
        extractEmail (EmailNotification e) = Just e
        extractEmail _                     = Nothing

e2a :: EmailAddress -> Address
e2a = Address Nothing . view unEmailAddress
