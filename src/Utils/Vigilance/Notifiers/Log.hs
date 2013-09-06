{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Utils.Vigilance.Notifiers.Log ( notify ) where

import ClassyPrelude
import Control.Lens
import Text.InterpolatedString.Perl6 (qc)

import Utils.Vigilance.Logger
import Utils.Vigilance.Types

notify :: LogNotifier
notify = LogNotifier notifierBody
  where notifierBody watches = renameLogCtx "Log Notifier" $ pushLogs formattedWatches >> return []
          where formattedWatches = map format watches
                format :: EWatch -> Text
                format w = [qc|Watch {w ^. watchName ^. unWatchName} notified.|]
