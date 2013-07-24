{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Utils.Vigilance.Web.Handlers (routes) where

import Utils.Vigilance.TableOps
import Utils.Vigilance.Types

import Control.Lens
import Control.Lens.TH
import Data.ByteString (ByteString)
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
         , ("watches/:id", method DELETE deleteWatchR) ]

app :: SnapletInit App App
app = makeSnaplet "vigilance" "Vigilence Web Server" Nothing $ do
    a <- nestSnaplet "acid" acid $ acidInit (AppState mempty) -- TODO: in-memory
    addRoutes routes
    return $ App a

createWatchR :: Handler App App ()
createWatchR = do
  watch  <- reqJSON
  ewatch <- update $ CreateWatchEvent watch
  writeJSON ewatch

deleteWatchR :: Handler App App ()
deleteWatchR = undefined
