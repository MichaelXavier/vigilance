{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
--TODO: split Logger and openLogger export into a separate module
module Utils.Vigilance.Notifiers.Log ( notify ) where

import ClassyPrelude
import Control.Lens
import Control.Monad.Reader (withReaderT)
import Data.Monoid (mconcat)
import Data.Text (Text)

import Utils.Vigilance.Logger
import Utils.Vigilance.Types

-- maybe error return type
--notify :: [EWatch] -> LogCtxT IO ()
notify watches = withReaderT newLogName $ pushLogs formattedWatches
  where formattedWatches = map format watches
        format :: EWatch -> Text
        format w = mconcat [ "Watch "
                           , w ^. watchName
                           , " failed to check in." ]
        newLogName ctx = ctx { ctxName = "Log Notifier" }
