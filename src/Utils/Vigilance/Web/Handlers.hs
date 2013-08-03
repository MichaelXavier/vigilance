{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TemplateHaskell       #-}
module Utils.Vigilance.Web.Handlers ( appInit
                                    , runServer) where

import Utils.Vigilance.TableOps
import Utils.Vigilance.Types

import ClassyPrelude
import Control.Lens
import Control.Lens.TH
import Control.Monad.Trans.Either (EitherT) --mehhh
import Control.Monad.Error.Class (throwError)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (readInt)
import Data.Maybe (fromJust)
import Data.Monoid (mempty)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Snap.Core
import Snap.Snaplet (serveSnaplet)
import Snap.Http.Server.Config (defaultConfig)
import Snap.Extras.JSON ( reqJSON
                        , writeJSON)
import Snap.Snaplet
import Snap.Snaplet.AcidState -- ( Acid
                               --- , getAcidState
                               --- , acidInit'
                               --- , HasAcid(..)
                               --- , update
                               --- , query)

data App = App { _acid :: Snaplet (Acid AppState) }

makeLenses ''App

instance HasAcid App AppState where
  getAcidStore app = app ^. (acid . snapletValue)

routes :: [(ByteString, Handler App App ())]
routes = [ ("watches", method POST createWatchR)
         , ("watches/:id", method DELETE deleteWatchR)
         , ("watches/:id", method GET findWatchR)
         , ("watches/:id/pause", method POST pauseWatchR)
         , ("watches/:id/unpause", method POST unPauseWatchR)
         , ("watches/:id/checkin", method POST checkInWatchR) ]

appInit :: Config -> MVar (AcidState AppState) -> SnapletInit App App
appInit cfg stVar = makeSnaplet "vigilance" "Vigilence Web Server" Nothing $ do
  a  <- nestSnaplet "acid" acid openAcid
  addPostInitHook stHook
  --addPostInitHookBase stHook
  --st <- getAcidState
  --liftIO $ putMVar stVar st
  addRoutes routes
  return $ App a
  where openAcid = acidInit' acidPath (AppState mempty) -- TODO: in-memory
        acidPath = cfg ^. configAcidPath
        --stHook :: Snaplet App -> EitherT Text IO (Snaplet App)
        --stHook v = undefined
        stHook v = do st <- lift $ getLens -- the fuck is going on here
                      let st' = st :: AcidState AppState
                      let wat = getAcidStore undefined
                      let wat' = wat :: AcidState AppState
                      liftIO $ putMVar stVar st
                      return v

runServer :: Config -> MVar (AcidState AppState) -> IO ()
runServer cfg = serveSnaplet defaultConfig . appInit cfg

createWatchR :: Handler App App ()
createWatchR = writeJSON =<< update . CreateWatchEvent =<< reqJSON

deleteWatchR :: Handler App App ()
deleteWatchR = update . DeleteWatchEvent =<< getID

findWatchR :: Handler App App ()
findWatchR = do mWatch <- query . FindWatchEvent =<< getID
                case mWatch of
                  Just w  -> writeJSON w
                  Nothing -> modifyResponse $ setResponseCode 404

pauseWatchR :: Handler App App ()
pauseWatchR = update . PauseWatchEvent =<< getID

unPauseWatchR :: Handler App App ()
unPauseWatchR = update =<< UnPauseWatchEvent <$> liftIO getPOSIXTime <*> getID

checkInWatchR :: Handler App App ()
checkInWatchR = update =<< CheckInWatchEvent <$> liftIO getPOSIXTime <*> getID

getID :: MonadSnap m => m ID
getID = handleFailure . toID =<<  getParam "id"
  where handleFailure = maybe pass return
        toID p = ID <$> (readInt' =<< p)

readInt' :: ByteString -> Maybe Int
readInt' = fmap fst . readInt
