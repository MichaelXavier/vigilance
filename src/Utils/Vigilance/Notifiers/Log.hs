{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils.Vigilance.Notifiers.Log ( notify ) where

import ClassyPrelude
import Control.Lens

import Utils.Vigilance.Logger
import Utils.Vigilance.Types

-- maybe error return type
notify :: [EWatch] -> LogCtxT IO ()
notify watches = renameLogCtx "Log Notifier" $ pushLogs formattedWatches
  where formattedWatches = map format watches
        format :: EWatch -> Text
        format w = mconcat [ "Watch "
                           , w ^. watchName ^. unWatchName
                           , " failed to check in." ]
