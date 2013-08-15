{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Utils.Vigilance.Notifiers.Email ( notify
                                       , generateEmails
                                       , EmailContext(..)
                                       , HasEmailContext(..)
                                       , Address(..)) where

import ClassyPrelude
import Control.Lens
import Control.Monad.Trans (lift)
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

data EmailContext = EmailContext { _fromEmail :: EmailAddress } deriving (Show, Eq)

makeClassy ''EmailContext

notify :: EmailContext -> Notifier
notify ctx watches = renameLogCtx "Email Notifier" $ mapM_ renderSendMail' mails
  where mails = generateEmails watches ctx

renderSendMail' :: Mail -> LogCtxT IO ()
renderSendMail' mail = do pushLog $ "Sending email notification to " <> emails
                          lift $ renderSendMail mail
  where emails = mconcat . map addressEmail . mailTo $ mail

generateEmails :: [EWatch] -> EmailContext -> [Mail]
generateEmails watches ctx = M.elems $ M.mapWithKey createMail groupedByEmail -- ehhhhh
  where groupedByEmail = M.fromListWith mappend $ concatMap watchesWithEmails watches
        createMail toAddr ws = (emptyMail from) { mailTo      = [e2a toAddr]
                                                , mailHeaders = [("Subject", subject)]
                                                , mailParts   = [[mailPart ws]] }
          where subject = [qc|Vigilence notification {watchCount} activated|] :: Text
                count
                  | watchCount > 1 = [qc|{watchCount} watches|] :: Text
                  | otherwise      = [qc|{watchCount} watch|] :: Text
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
                name     = w ^. watchName . to unpack

watchesWithEmails :: EWatch -> [(EmailAddress, [EWatch])]
watchesWithEmails w = zip emails (repeat [w] :: [[EWatch]])
  where emails = catMaybes $ map extractEmail $ w ^. watchNotifications
        extractEmail (EmailNotification e) = Just e
        extractEmail _                     = Nothing

e2a :: EmailAddress -> Address
e2a = Address Nothing . view unEmailAddress
