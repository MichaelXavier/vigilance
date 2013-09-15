{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoImplicitPrelude         #-}
-- thanks, yesod :(
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Utils.Vigilance.Web.Yesod ( runServer
                                 , WebApp(..) ) where

import ClassyPrelude
import Control.Lens
import Control.Monad ( (<=<) )
import Data.Acid (AcidState)
import Data.Time.Clock.POSIX ( getPOSIXTime
                             , POSIXTime )
import qualified Network.Wai as W
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types.Status (noContent204)
import Network.Wai.Middleware.Autohead
import Network.Wai.Middleware.AcceptOverride
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.MethodOverride


import Utils.Vigilance.Config (configNotifiers)
import Utils.Vigilance.Logger ( runInLogCtx
                              , vLog )
import Utils.Vigilance.TableOps
import Utils.Vigilance.Types
import Utils.Vigilance.Workers.NotificationWorker (sendNotifications)
import Utils.Vigilance.Utils (bindM3)

import System.Log.FastLogger (LogStr(..))
import Yesod

data WebApp = WebApp { _acid    :: AcidState AppState
                     , _cfg     :: Config
                     , _logChan :: LogChan }

makeClassy ''WebApp

logCtxName :: Text
logCtxName = "Web"

-- stolen form yesod implementation. this needs to be public

lSToT :: LogStr -> Text
lSToT (LS s) = pack s
lSToT (LB s) = decodeUtf8 s

instance Yesod WebApp where
  makeSessionBackend = const $ return Nothing

mkYesod "WebApp" [parseRoutes|
  /watches                    WatchesR      GET
  /watches/#WatchName         WatchR        GET DELETE
  /watches/#WatchName/pause   PauseWatchR   POST
  /watches/#WatchName/unpause UnPauseWatchR POST
  /watches/#WatchName/checkin CheckInWatchR POST
  /watches/#WatchName/test    TestWatchR    POST
|]

getWatchesR :: Handler Value
getWatchesR = returnJson =<< allWatchesS =<< getDb

getWatchR :: WatchName -> Handler Value
getWatchR =  jsonOrNotFound <=< onWatch findWatchS

deleteWatchR :: WatchName -> Handler Value
deleteWatchR name = onWatchExists deleteWatchS name >> noContent

postPauseWatchR :: WatchName -> Handler Value
postPauseWatchR name = onWatchExists pauseWatchS name >> noContent

postUnPauseWatchR :: WatchName -> Handler Value
postUnPauseWatchR name = onWatchExists unPause name >> noContent
  where unPause db n = liftIO $ bindM3 unPauseWatchS (return db) getPOSIXTime (return n)

postCheckInWatchR :: WatchName -> Handler Value
postCheckInWatchR name = onWatchExists checkIn name >> noContent
  where checkIn db n = liftIO $ bindM3 checkInWatchS (return db) getPOSIXTime (return n)

postTestWatchR :: WatchName -> Handler Value
postTestWatchR = maybe notFound doTest <=< onWatch findWatchS
  where doTest w = do notifiers <- configNotifiers <$> getConfig
                      returnJson =<< inWebLogCtx (sendNotifications [w] notifiers)

noContent :: Handler Value
noContent = sendResponseStatus noContent204 ()

alwaysNoContent :: a -> Handler Value
alwaysNoContent = const noContent

jsonOrNotFound :: ToJSON a => Maybe a -> Handler Value
jsonOrNotFound = maybe notFound returnJson

runServer :: WebApp -> IO ()
runServer w = run port =<< toWaiApp' w
  where port = w ^. cfg . configPort

toWaiApp' :: WebApp -> IO W.Application
toWaiApp' site = do
  let ctx = LogCtx logCtxName $ site ^. logChan
  middleware <- mkDefaultMiddlewares' ctx
  middleware <$> toWaiAppPlain site

mkDefaultMiddlewares' :: LogCtx -> IO W.Middleware
mkDefaultMiddlewares' ctx = do
    logWare <- mkRequestLogger def
        { destination = Callback cb
        , outputFormat = Apache FromSocket
        }
    return $ logWare
           . acceptOverride
           . autohead
           . gzip def
           . methodOverride
  where cb = runInLogCtx ctx . vLog . mconcat . map lSToT

getDb :: HandlerT WebApp IO (AcidState AppState)
getDb = view acid <$> getYesod

inWebLogCtx :: LogCtxT IO a -> HandlerT WebApp IO a
inWebLogCtx action = do
  ctx <- LogCtx <$> pure logCtxName <*> getLogChan
  liftIO $ runInLogCtx ctx action

getLogChan :: HandlerT WebApp IO LogChan
getLogChan = view logChan <$> getYesod

getConfig :: HandlerT WebApp IO Config
getConfig = view cfg <$> getYesod

onWatch :: (AcidState AppState -> WatchName -> HandlerT WebApp IO b) -> WatchName -> HandlerT WebApp IO b
onWatch f n = join $ f <$> getDb <*> pure n

onWatchExists :: (AcidState AppState -> WatchName -> HandlerT WebApp IO b) -> WatchName -> HandlerT WebApp IO b
onWatchExists f n = maybe notFound goAhead =<< checkExistence
  where checkExistence = do db <- getDb
                            liftIO $ findWatchS db n
        goAhead        = const $ onWatch f n

getPOSIXTime' :: MonadIO m => m POSIXTime
getPOSIXTime' = liftIO getPOSIXTime
