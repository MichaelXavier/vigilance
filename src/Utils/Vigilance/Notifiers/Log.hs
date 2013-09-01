{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils.Vigilance.Notifiers.Log ( notify ) where

import ClassyPrelude
import Control.Lens

import Utils.Vigilance.Logger
import Utils.Vigilance.Types

-- maybe error return type
notify :: Notifier
notify watches = renameLogCtx "Log Notifier" $ pushLogs formattedWatches >> return []
  where formattedWatches = map format watches
        format :: EWatch -> Text
        format w = mconcat [ "Watch "
                           , w ^. watchName ^. unWatchName
                           , " notified." ]
