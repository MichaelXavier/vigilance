{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoImplicitPrelude         #-}
module Utils.Vigilance.Web.Yesod ( runServer
                                 , WebApp(..) ) where

import ClassyPrelude
import Control.Applicative ( (<$>)
                           , (<*>)
                           , pure )
import Control.Lens
import Control.Monad ( (<=<)
                     , join )
import Data.Acid (AcidState)
import qualified Data.Acid as A
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types.Status (noContent204)
import Yesod

import Utils.Vigilance.TableOps
import Utils.Vigilance.Types
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
|]

getWatchesR :: Handler Value
getWatchesR = returnJson =<< allWatchesS =<< getDb

postWatchesR :: Handler Value
postWatchesR = returnJson =<< bindM2 createWatchS getDb parseJsonBody_

getWatchR :: WatchName -> Handler Value
getWatchR =  jsonOrNotFound <=< onWatch findWatchS

deleteWatchR :: WatchName -> Handler Value
deleteWatchR = (const noContent) <=< onWatch deleteWatchS

postPauseWatchR :: WatchName -> Handler Value
postPauseWatchR = (const noContent) <=< onWatch pauseWatchS

postUnPauseWatchR :: WatchName -> Handler Value
postUnPauseWatchR = (const noContent) <=< bindM3 unPauseWatchS getDb getPOSIXTime' . return

postCheckInWatchR :: WatchName -> Handler Value
postCheckInWatchR = (const noContent) <=< bindM3 checkInWatchS getDb getPOSIXTime' . return

--TODO: type
noContent = sendResponseStatus noContent204 ()

jsonOrNotFound = maybe notFound returnJson

runServer :: WebApp -> IO ()
runServer w = run port =<< toWaiApp w
  where port = w ^. cfg . configPort

getDb = view acid <$> getYesod --might be able to use `use`

-- TODO: handle Nothing with 404 
onWatch f i = join $ f <$> getDb <*> pure i

getPOSIXTime' = liftIO getPOSIXTime
