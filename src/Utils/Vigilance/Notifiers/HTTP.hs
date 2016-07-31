{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
module Utils.Vigilance.Notifiers.HTTP ( notify
                                      , watchesWithNotifications ) where

import ClassyPrelude
import Control.Lens
import Blaze.ByteString.Builder (Builder)
import Data.Aeson ( ToJSON
                  , encode )
import Data.Ix (inRange)
import Data.String.Conversions (cs)
import Network.Http.Client ( URL
                           , inputStreamBody
                           , getStatusCode
                           , post )
import qualified System.IO.Streams as S
import Text.InterpolatedString.Perl6 (qc)
import Utils.Vigilance.Logger ( renameLogCtx
                              , pushLog
                              , vLog )
import Utils.Vigilance.Types

notify :: HTTPNotifier
notify = HTTPNotifier notifierBody
  where notifierBody watches = renameLogCtx "HTTP Notifier" $ catMaybes <$> mapM (uncurry makeRequest) notifications
          where notifications = watchesWithNotifications watches

watchesWithNotifications :: [EWatch] -> [(EWatch, URL)]
watchesWithNotifications = concatMap extractUrls
  where extractUrls w = zip (repeat w :: [EWatch]) urls
          where urls = [ u | HTTPNotification u <- w ^. watchNotifications]

makeRequest :: EWatch -> URL -> LogCtxT IO (Maybe FailedNotification)
makeRequest w url = do
  inputStream <- lift $ jsonBodyStream w
  vLog [qc|Notifying {wn} at {url}|]
  result <- lift . tryAny $ post url "application/json" inputStream skipResponse
  either failedByException handleCode result
  where skipResponse r _ = return $ getStatusCode r
        wn               = w ^. watchName . unWatchName
        handleCode code
          | inRange (200, 299) code = success code
          | otherwise               = failure code
        success code = vLog [qc|{url} returned {code}|] >> return Nothing
        failure code = pushLog [qc|{url} failed with {code}|] >> failedByCode code
        failedByException e = return . Just $ FailedNotification w notif (FailedByException . cs . show $ e) 0
        failedByCode code   = return . Just $ FailedNotification w notif (FailedByCode code) 0
        notif = HTTPNotification url

jsonBodyStream :: ToJSON a => a -> IO (S.OutputStream Blaze.ByteString.Builder.Builder -> IO ())
jsonBodyStream = fmap inputStreamBody . S.fromLazyByteString . encode
