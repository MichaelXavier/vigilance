{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils.Vigilance.Workers.NotificationRetryWorker ( runWorker ) where

import ClassyPrelude
import Control.Lens

runWorker :: AcidState AppState -> NotifierGroup -> LogCtxT IO ()
runWorker acid notifiers = undefined
