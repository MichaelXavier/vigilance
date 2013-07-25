{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TemplateHaskell       #-}
module Utils.Vigilance.Web.Handlers (routes) where

import Utils.Vigilance.TableOps
import Utils.Vigilance.Types

import ClassyPrelude
import Control.Lens
import Control.Lens.TH
import Control.Monad.Error.Class (throwError)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (readInt)
import Data.Maybe (fromJust)
import Data.Monoid (mempty)
import Snap.Core
import Snap.Extras.JSON ( reqJSON
                        , writeJSON)
import Snap.Snaplet
import Snap.Snaplet.AcidState ( Acid
                              , acidInit
                              , HasAcid(..)
                              , update
                              , query)

data App = App { _acid :: Snaplet (Acid AppState) }

makeLenses ''App

instance HasAcid App AppState where
  getAcidStore app = app ^. (acid . snapletValue)

routes :: [(ByteString, Handler App App ())]
routes = [ ("watches", method POST createWatchR)
         , ("watches/:id", method DELETE deleteWatchR)
         , ("watches/:id", method GET findWatchR) ]

app :: SnapletInit App App
app = makeSnaplet "vigilance" "Vigilence Web Server" Nothing $ do
    a <- nestSnaplet "acid" acid $ acidInit (AppState mempty) -- TODO: in-memory
    addRoutes routes
    return $ App a

createWatchR :: Handler App App ()
createWatchR = writeJSON =<< update . CreateWatchEvent =<< reqJSON

deleteWatchR :: Handler App App ()
deleteWatchR = update . DeleteWatchEvent =<< getID

findWatchR :: Handler App App ()
findWatchR = do mWatch <- query . FindWatchEvent =<< getID
                case mWatch of
                  Just w  -> writeJSON w
                  Nothing -> modifyResponse $ setResponseCode 404

getID :: MonadSnap m => m ID
getID = do p <- getParam "id"
           let i = ID <$> (readInt' =<< p)
           maybe pass return i

readInt' :: ByteString -> Maybe Int
readInt' = fmap fst . readInt
