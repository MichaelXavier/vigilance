{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Utils.Vigilance.Web.Handlers (routes) where

import Utils.Vigilance.Types

import Control.Lens
import Control.Lens.TH
import Snap.Core
import Snap.Extras.JSON (reqJSON)
import Snap.Snaplet
import Snap.Snaplet.AcidState

data App = App { _acid :: Snaplet (Acid AppState) }

makeLenses ''App

instance HasAcid App AppState where
  getAcidStore app = app ^. (acid . snapletValue)

routes = route [ ("watches", method POST createWatchR)
               , ("watches/:id", method DELETE deleteWatchR) ]

createWatchR :: Snap ()
createWatchR = undefined --do
  --watch <- reqJSON
  --createWatch watch table -- TODO: send back location, render EWatch

deleteWatchR :: Snap ()
deleteWatchR = undefined
