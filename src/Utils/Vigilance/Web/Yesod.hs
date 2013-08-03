{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Utils.Vigilance.Web.Yesod ( runServer
                                 , WebApp(..) ) where

import Control.Lens
import Control.Monad ((<=<))
import Data.Acid (AcidState)
import Network.Wai.Handler.Warp (run)
import Yesod

import Utils.Vigilance.Types

data WebApp = WebApp { _acid :: AcidState AppState
                     , _logChan :: LogChan }

makeClassy ''WebApp

instance Yesod WebApp where
  makeSessionBackend = const $ return Nothing

mkYesod "WebApp" [parseRoutes|
  /watches             WatchesR      GET POST
  /watches/#ID         WatchR        GET DELETE
  /watches/#ID/pause   PauseWatchR   POST
  /watches/#ID/unpause UnPauseWatchR POST
  /watches/#ID/checkin CheckInWatchR POST
|]

getWatchesR :: Handler Value
getWatchesR = undefined

postWatchesR :: Handler Value
postWatchesR = undefined

getWatchR :: ID -> Handler Value
getWatchR = undefined

deleteWatchR :: ID -> Handler Value
deleteWatchR = undefined

postPauseWatchR :: ID -> Handler Value
postPauseWatchR = undefined

postUnPauseWatchR :: ID -> Handler Value
postUnPauseWatchR = undefined

postCheckInWatchR :: ID -> Handler Value
postCheckInWatchR = undefined

runServer :: WebApp -> IO ()
runServer = run 3000 <=< toWaiApp
