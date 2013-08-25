{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Utils.Vigilance.Client.Client ( getList
                                     , displayList
                                     , VError(..) ) where

import ClassyPrelude
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

import Utils.Vigilance.Client.Config
import Utils.Vigilance.Types


displayList :: [EWatch] -> IO ()
displayList = putStrLn . renderList

--TODO: better rendering
renderList :: [EWatch] -> Text
renderList = unlines . map renderWatch
  where renderWatch = show

getList :: ClientCtxT IO (VigilanceResponse [EWatch])
getList = makeRequest GET "/watches" emptyBody


makeRequest :: FromJSON a
            => Method
            -> ByteString
            -> (S.OutputStream Builder -> IO b)
            -> ClientCtxT IO (VigilanceResponse a)
makeRequest m p body = do host <- asks serverHost
                          port <- asks serverPort
                          lift $ do
                            withConnection (openConnection host port) $ \c -> do 
                              req <- buildRequest $ do
                                      http m p
                                      setAccept "application/json"
                                      setUserAgent defaultUserAgent
                              sendRequest c req body
                              receiveResponse c jsonResponseHandler

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
