{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
module Utils.Vigilance.Client.Client ( getList
                                     , getInfo
                                     , pause
                                     , unPause
                                     , checkIn
                                     , test
                                     , displayList
                                     , displayWatch
                                     , displayWatchInfo
                                     , displayFailedNotifications
                                     -- for testing
                                     , renderList
                                     , renderWatch
                                     , renderWatchInfo
                                     , renderFailedNotifications
                                     , VError(..) ) where

import ClassyPrelude
import Control.Lens
import Control.Monad.Trans.Reader (asks)
import Data.Aeson ( FromJSON
                  , json
                  , Result(..)
                  , fromJSON )
import Blaze.ByteString.Builder (Builder)
import Data.Ix (inRange)
import Data.String.Conversions (cs)
import Network.Http.Client ( Method(GET, POST)
                           , Response
                           , emptyBody
                           , withConnection
                           , openConnection
                           , http
                           , setAccept
                           , setHeader
                           , buildRequest
                           , sendRequest
                           , receiveResponse
                           , RequestBuilder
                           , getStatusCode
                           , StatusCode )
import System.IO.Streams.Attoparsec (parseFromStream)
import qualified System.IO.Streams as S
import Text.InterpolatedString.Perl6 (qc)

import Utils.Vigilance.Client.Config
import Utils.Vigilance.Types


displayList :: [EWatch] -> IO ()
displayList = putStrLn . renderList

renderList :: [EWatch] -> Text
renderList = unlines' . map renderWatch

displayWatch :: EWatch -> IO ()
displayWatch = putStrLn . renderWatch

renderWatch :: EWatch -> Text
renderWatch w = [qc|{name} ({i}) - {interval} - {state}|]
  where name     = w ^. watchName . unWatchName
        i        = w ^. watchId . unID
        interval = w ^. watchInterval
        state    = w ^. watchWState . to renderState

displayWatchInfo :: EWatch -> IO ()
displayWatchInfo = putStrLn . renderWatchInfo

displayFailedNotifications :: [FailedNotification] -> IO ()
displayFailedNotifications = putStrLn . renderFailedNotifications

renderFailedNotifications :: [FailedNotification] -> Text
renderFailedNotifications []  = "All notifications sent successfully."
renderFailedNotifications fns = unlines' . (header:) . map render $ fns
  where header = "The following errors were encountered when testing:"
        render fn = [qc|- {pref} for {wn} ({wid}): {err}|]
          where pref = fn ^. failedPref . to renderNotificationPref
                wn   = fn ^. failedWatch . watchName . unWatchName
                wid  = fn ^. failedWatch . watchId . unID
                err  = fn ^. failedLastError . to renderNotificationError

renderNotificationPref :: NotificationPreference -> Text
renderNotificationPref (HTTPNotification u)                 = [qc|HTTP Notification ({u})|]
renderNotificationPref (EmailNotification (EmailAddress a)) = [qc|Email Notification ({a})|]

renderNotificationError :: NotificationError -> Text
renderNotificationError (FailedByCode c)      = [qc|Failed with status code {c}|]
renderNotificationError (FailedByException e) = [qc|Failed with exception "{e}"|]

renderWatchInfo :: EWatch -> Text
renderWatchInfo w = [qc|{renderedWatch}

Notifications:
{renderedNotifications}|]
  where renderedWatch = renderWatch w
        notifications = w ^. watchNotifications
        renderedNotifications
          | null notifications = bullet "none"
          | otherwise          = unlines' . map (bullet . renderNotification) $ notifications

renderNotification :: NotificationPreference -> Text
renderNotification (EmailNotification (EmailAddress a)) = [qc|Email: {a}|]
renderNotification (HTTPNotification u)                 = [qc|HTTP: {u}|]

renderState :: WatchState -> Text
renderState (Active t) = [qc|Active {t}|]
renderState x          = cs (show x)

bullet :: Text -> Text
bullet x = [qc| - {x}|]

getList :: ClientCtxT IO (VigilanceResponse [EWatch])
getList = makeRequest GET "/watches" emptyBody

getInfo :: WatchName -> ClientCtxT IO (VigilanceResponse EWatch)
getInfo n = makeRequest GET (watchRoute n) emptyBody

pause :: WatchName -> ClientCtxT IO (VigilanceResponse ())
pause n = makeRequest_ POST (watchRoute n <> "/pause") emptyBody

unPause :: WatchName -> ClientCtxT IO (VigilanceResponse ())
unPause n = makeRequest_ POST (watchRoute n <> "/unpause") emptyBody

checkIn :: WatchName -> ClientCtxT IO (VigilanceResponse ())
checkIn n = makeRequest_ POST (watchRoute n <> "/checkin") emptyBody

test :: WatchName -> ClientCtxT IO (VigilanceResponse [FailedNotification])
test n = makeRequest POST (watchRoute n <> "/test") emptyBody

watchRoute :: WatchName -> ByteString
watchRoute (WatchName n) = "/watches/" <> encodeUtf8 n


makeRequest_ :: Method
                -> ByteString
                -> (S.OutputStream Blaze.ByteString.Builder.Builder -> IO b)
                -> ClientCtxT IO (VigilanceResponse ())
makeRequest_ = makeRequest' unitResponseHandler

makeRequest :: FromJSON a
               => Method
               -> ByteString
               -> (S.OutputStream Blaze.ByteString.Builder.Builder -> IO b)
               -> ClientCtxT IO (VigilanceResponse a)
makeRequest = makeRequest' jsonResponseHandler

makeRequest':: (Response -> S.InputStream ByteString -> IO (VigilanceResponse a))
               -> Method
               -> ByteString
               -> (S.OutputStream Blaze.ByteString.Builder.Builder -> IO b)
               -> ClientCtxT IO (VigilanceResponse a)
makeRequest' handler m p body = do
  host <- asks serverHost
  port <- asks serverPort
  lift $ withConnection (openConnection host port) $ \c -> do 
      req <- buildRequest $ do
              http m p
              setAccept "application/json"
              setUserAgent defaultUserAgent
      void $ sendRequest c req body
      receiveResponse c handler

setUserAgent :: ByteString -> RequestBuilder ()
setUserAgent = setHeader "User-Agent"

defaultUserAgent :: ByteString
defaultUserAgent = "vigilance client"

unitResponseHandler :: Response
                       -> S.InputStream ByteString
                       -> IO (VigilanceResponse ())
unitResponseHandler = responseHandler (const $ return $ Right ())

jsonResponseHandler :: FromJSON a
                       => Response
                       -> S.InputStream ByteString
                       -> IO (VigilanceResponse a)
jsonResponseHandler = responseHandler handleJSONBody

responseHandler :: (S.InputStream ByteString -> IO (VigilanceResponse a))
                   -> Response
                   -> S.InputStream ByteString
                   -> IO (VigilanceResponse a)
responseHandler successHandler resp stream
  | responseOk        = successHandler stream
  | notFound          = return . Left $ NotFound 
  | otherwise         = return . Left $ StatusError statusCode
  where statusCode        = getStatusCode resp
        responseOk        = inRange (200, 299) statusCode
        notFound          = statusCode == 404
  

handleJSONBody :: FromJSON a => S.InputStream ByteString -> IO (VigilanceResponse a)
handleJSONBody stream = coerceParsed <$> parseJSONBody stream

data VError = NotFound        |
              ParseError Text |
              StatusError StatusCode deriving (Show, Eq)

parseJSONBody :: FromJSON a => S.InputStream ByteString -> IO (Result a)
parseJSONBody = parseFromStream parser
  where parser = fmap fromJSON json

type VigilanceResponse a = Either VError a

coerceParsed :: Result a -> VigilanceResponse a
coerceParsed (Success a) = Right a
coerceParsed (Error e)   = Left $ ParseError $ pack e

unlines' :: [Text] -> Text
unlines' = intercalate "\n"
