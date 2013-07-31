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
import Control.Monad.Reader (ReaderT, ask)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Lazy as M
import Data.String.Here.Interpolated (iTrim)
import Network.Mail.Mime ( Address(..)
                         , emptyMail
                         , Part(..)
                         , renderSendMail
                         , Encoding( QuotedPrintableText )
                         , Mail(..))

import Utils.Vigilance.Types

data EmailContext = EmailContext { _fromEmail :: EmailAddress } deriving (Show, Eq)

makeClassy ''EmailContext

notify :: [EWatch] -> ReaderT EmailContext IO ()
notify watches = do mails <- generateEmails <$> pure watches <*> ask
                    lift $ mapM_ renderSendMail mails

generateEmails :: [EWatch] -> EmailContext -> [Mail]
generateEmails watches ctx = M.elems $ M.mapWithKey createMail groupedByEmail -- ehhhhh
  where groupedByEmail = M.fromListWith mappend $ concatMap watchesWithEmails watches
        createMail toAddr ws = (emptyMail from) { mailTo      = [e2a toAddr]
                                                , mailHeaders = [("Subject", subject)]
                                                , mailParts   = [[mailPart ws]] }
          where subject = [iTrim|Vigilence notification ${watchCount} activated|]
                count
                  | watchCount > 1 = [iTrim|$(watchCount) watches|]
                  | otherwise      = [iTrim|$(watchCount) watch|]
                watchCount = length ws
        from :: Address
        from = e2a $ ctx ^. fromEmail -- ehhhhh

mailPart :: [EWatch] -> Part
mailPart ws =  Part "text/plain; charset=utf-8" QuotedPrintableText Nothing [] (bodyLBS ws)

bodyLBS :: [EWatch] -> LBS.ByteString
bodyLBS ws = [iTrim|
The following watches were triggered:

${watchSummary}

Sincerely,
Vigilence
  |]
  where watchSummary = mconcat $ map summarize ws
        summarize w  = [iTrim|- ${name} (${interval})|] :: LBS.ByteString
          where interval = w ^. watchInterval
                name     = w ^. watchName . to unpack

watchesWithEmails :: EWatch -> [(EmailAddress, [EWatch])]
watchesWithEmails w = zip emails (repeat [w] :: [[EWatch]])
  where emails = catMaybes $ map extractEmail $ w ^. watchNotifications
        extractEmail (EmailNotification e) = Just e
        extractEmail _                     = Nothing

e2a :: EmailAddress -> Address
e2a = Address Nothing . view unEmailAddress
