{-# LANGUAGE OverloadedStrings #-}
module Utils.Vigilance.Workers.StaticWatchWorker (runWorker) where

import Data.Acid (AcidState)
import qualified Data.Configurator.Types as CT
import Control.Applicative ((<$>))
import Control.Monad (forever) -- why :(
import Control.Lens
import Data.Text (Text)
import Utils.Vigilance.Config ( reloadConfig
                              , convertConfig )
import Utils.Vigilance.Logger (pushLog)
import Utils.Vigilance.TableOps (mergeStaticWatchesS)
import Utils.Vigilance.Types
import Utils.Vigilance.Utils ( WakeSig
                             , waitForWake )

runWorker :: AcidState AppState -> LogChan -> CT.Config -> WakeSig () -> IO ()
runWorker acid logChan cfg wakeSig = forever $ do -- why? :(
  pushLog' "Waiting on HUP signal for config reload"
  waitForWake wakeSig
  pushLog' "Caught HUP signal. Reloading config"
  reloadConfig cfg
  watches <- view configWatches <$> convertConfig cfg
  pushLog' "Merging static watches"
  mergeStaticWatchesS acid watches
  pushLog' "Static watches merged"
  where pushLog' :: Text -> IO ()
        pushLog' = pushLog logChan
