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
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types.Status (noContent204)
import Yesod

import Utils.Vigilance.Config (configNotifiers)
import Utils.Vigilance.Logger (runInLogCtx)
import Utils.Vigilance.TableOps
import Utils.Vigilance.Types
import Utils.Vigilance.Workers.NotificationWorker (sendNotificationsWithRetry)
import Utils.Vigilance.Utils ( bindM2
                             , bindM3 )

data WebApp = WebApp { _acid    :: AcidState AppState
                     , _cfg     :: Config
                     , _logChan :: LogChan }

makeClassy ''WebApp

instance Yesod WebApp where
  makeSessionBackend = const $ return Nothing

mkYesod "WebApp" [parseRoutes|
  /watches                    WatchesR      GET POST
  /watches/#WatchName         WatchR        GET DELETE
  /watches/#WatchName/pause   PauseWatchR   POST
  /watches/#WatchName/unpause UnPauseWatchR POST
  /watches/#WatchName/checkin CheckInWatchR POST
  /watches/#WatchName/test    TestWatchR POST
|]

getWatchesR :: Handler Value
getWatchesR = returnJson =<< allWatchesS =<< getDb

postWatchesR :: Handler Value
postWatchesR = returnJson =<< bindM2 createWatchS getDb parseJsonBody_

getWatchR :: WatchName -> Handler Value
getWatchR =  jsonOrNotFound <=< onWatch findWatchS

deleteWatchR :: WatchName -> Handler Value
deleteWatchR = alwaysNoContent <=< onWatchExists deleteWatchS

postPauseWatchR :: WatchName -> Handler Value
postPauseWatchR = alwaysNoContent <=< onWatchExists pauseWatchS

--TODO; handle these with 404s
postUnPauseWatchR :: WatchName -> Handler Value
postUnPauseWatchR = alwaysNoContent <=< bindM3 unPauseWatchS getDb getPOSIXTime' . return

postCheckInWatchR :: WatchName -> Handler Value
postCheckInWatchR = alwaysNoContent <=< bindM3 checkInWatchS getDb getPOSIXTime' . return

--TODO: reflect failures in status
postTestWatchR :: WatchName -> Handler Value
postTestWatchR = maybe notFound doTest <=< onWatch findWatchS -- TODO: DRY up
  where doTest w = do notifiers <- configNotifiers <$> getConfig
                      db        <- getDb
                      inWebLogCtx $ sendNotificationsWithRetry db [w] notifiers
                      noContent

noContent :: Handler Value
noContent = sendResponseStatus noContent204 ()

alwaysNoContent :: a -> Handler Value
alwaysNoContent = const noContent

jsonOrNotFound :: ToJSON a => Maybe a -> Handler Value
jsonOrNotFound = maybe notFound returnJson

runServer :: WebApp -> IO ()
runServer w = run port =<< toWaiApp w
  where port = w ^. cfg . configPort

getDb :: HandlerT WebApp IO (AcidState AppState)
getDb = view acid <$> getYesod

inWebLogCtx :: LogCtxT IO a -> HandlerT WebApp IO a
inWebLogCtx action = do
  ctx <- LogCtx <$> pure "Web" <*> getLogChan
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
