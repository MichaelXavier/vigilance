{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Utils.Vigilance.Client.Client ( getList
                                     , getInfo
                                     , pause
                                     , unPause
                                     , checkIn
                                     , displayList
                                     , displayWatch
                                     , displayWatchInfo
                                     -- for testing
                                     , renderList
                                     , renderWatch
                                     , renderWatchInfo
                                     , VError(..) ) where

import ClassyPrelude
import Control.Lens
import Control.Monad ((<=<))
import Control.Monad.Trans.Reader (asks)
import Data.Aeson ( FromJSON
                  , json
                  , Result(..)
                  , fromJSON )
import Blaze.ByteString.Builder (Builder)
import Data.Ix (inRange)
--TODO: limit impors
import Network.Http.Client
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

renderState :: WatchState -> Text
renderState (Active t) = [qc|Active {t}|]
renderState x          = show x

bullet :: Text -> Text
bullet x = [qc| - {x}|]

getList :: ClientCtxT IO (VigilanceResponse [EWatch])
getList = makeRequest GET "/watches" emptyBody

getInfo :: WatchName -> ClientCtxT IO (VigilanceResponse EWatch)
getInfo n = makeRequest GET (watchRoute n) emptyBody

pause :: WatchName -> ClientCtxT IO (VigilanceResponse ())
pause n = makeRequest_ POST (watchRoute n <> "/pause") emptyBody

unPause :: WatchName -> ClientCtxT IO (VigilanceResponse ())
unPause n = makeRequest POST (watchRoute n <> "/unpause") emptyBody

checkIn :: WatchName -> ClientCtxT IO (VigilanceResponse ())
checkIn n = makeRequest POST (watchRoute n <> "/checkin") emptyBody

watchRoute :: WatchName -> ByteString
watchRoute (WatchName n) = "/watches/" <> encodeUtf8 n

-- Workaround because touching the response body on a 204 hangs forever
-- currently with http-streams
makeRequest_ :: Method
                -> ByteString
                -> (S.OutputStream Builder -> IO b)
                -> ClientCtxT IO (VigilanceResponse ())
makeRequest_ = makeRequest' ignoreResponse
  where ignoreResponse _ = return . Right $ ()

makeRequest :: FromJSON a
               => Method
               -> ByteString
               -> (S.OutputStream Builder -> IO b)
               -> ClientCtxT IO (VigilanceResponse a)
makeRequest = makeRequest' responseHandler
  where responseHandler c = receiveResponse c jsonResponseHandler

makeRequest' :: FromJSON a
                => (Connection -> IO (VigilanceResponse a))
                -> Method
                -> ByteString
                -> (S.OutputStream Builder -> IO b)
                -> ClientCtxT IO (VigilanceResponse a)
makeRequest' responseHandler m p body = do
  host <- asks serverHost
  port <- asks serverPort
  lift $ do
    withConnection (openConnection host port) $ \c -> do 
      req <- buildRequest $ do
              http m p
              setAccept "application/json"
              setUserAgent defaultUserAgent
      sendRequest c req body
      responseHandler c

setUserAgent :: ByteString -> RequestBuilder ()
setUserAgent = setHeader "User-Agent"

defaultUserAgent :: ByteString
defaultUserAgent = "vigilance client"

--TODO: look at response and handle non-200
jsonResponseHandler :: (FromJSON a)
                       => Response
                       -> S.InputStream ByteString
                       -> IO (VigilanceResponse a)
jsonResponseHandler resp stream
  | responseOk = handleJSONBody stream
  | notFound   = return . Left $ NotFound 
  | otherwise  = return . Left $ StatusError statusCode
  where statusCode = getStatusCode resp
        responseOk = inRange (200, 299) statusCode
        notFound   = statusCode == 404

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
