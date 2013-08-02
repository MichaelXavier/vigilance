{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
--TODO: split Logger and openLogger export into a separate module
module Utils.Vigilance.Notifiers.Log ( notify ) where

import ClassyPrelude
import Control.Lens
import Data.Monoid (mconcat)
import System.Log.FastLogger ( LogStr(LB) ) --todo: reexport from types

import Utils.Vigilance.Logger
import Utils.Vigilance.Types

-- maybe error return type
notify :: LogChan -> Notifier
notify q watches = pushLogs q formattedWatches
  where formattedWatches = map format watches
        format w         = LB $ mconcat ["Watch "
                                        , w ^. watchName . to encodeUtf8
                                        , " failed to check in." ]
