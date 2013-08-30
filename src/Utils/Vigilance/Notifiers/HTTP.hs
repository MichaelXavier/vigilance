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
import Network.Http.Client ( URL
                           , inputStreamBody
                           , getStatusCode
                           , post )
import qualified System.IO.Streams as S
import Text.InterpolatedString.Perl6 (qc)
import Utils.Vigilance.Logger ( renameLogCtx
                              , vLog )
import Utils.Vigilance.Types

notify :: Notifier
notify watches = renameLogCtx "HTTP Notifier" $ mapM_ (uncurry makeRequest) notifications
  where notifications = watchesWithNotifications watches

watchesWithNotifications :: [EWatch] -> [(EWatch, URL)]
watchesWithNotifications = concatMap extractUrls
  where extractUrls w = zip (repeat w :: [EWatch]) $ mapMaybe extractUrl $ w ^. watchNotifications
        extractUrl (HTTPNotification u) = Just u
        extractUrl _                    = Nothing


makeRequest :: EWatch -> URL -> LogCtxT IO ()
makeRequest w url = do
  inputStream <- lift $ jsonBodyStream w
  vLog [qc|Notifying {w ^. watchName} at {url}|]
  code <- lift $ post url "application/json" inputStream skipResponse
  vLog [qc|{url} returned {code}|]
  where skipResponse r _ = return $ getStatusCode r

jsonBodyStream :: ToJSON a => a -> IO (S.OutputStream Builder -> IO ())
jsonBodyStream x = inputStreamBody <$> (S.fromLazyByteString $ encode x)
