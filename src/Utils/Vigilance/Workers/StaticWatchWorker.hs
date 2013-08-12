{-# LANGUAGE OverloadedStrings #-}
module Utils.Vigilance.Workers.StaticWatchWorker (runWorker) where

import Data.Acid (AcidState)
import qualified Data.Configurator.Types as CT
import Control.Applicative ((<$>))
import Control.Monad (forever) -- why :(
import Control.Monad.Reader (withReaderT)
import Control.Monad.Trans (lift)
import Control.Lens
import Data.Text (Text)
import Utils.Vigilance.Config ( reloadConfig
                              , convertConfig )
import Utils.Vigilance.Logger (pushLog)
import Utils.Vigilance.TableOps (mergeStaticWatchesS)
import Utils.Vigilance.Types
import Utils.Vigilance.Utils ( WakeSig
                             , waitForWake )

runWorker :: AcidState AppState -> CT.Config -> WakeSig () -> LogCtxT IO ()
runWorker acid cfg wakeSig = withReaderT newLogName $ forever $ do -- why forever? :(
  pushLog "Waiting on HUP signal for config reload"
  lift $ waitForWake wakeSig
  pushLog "Caught HUP signal. Reloading config"
  lift $ reloadConfig cfg
  watches <- lift $ view configWatches <$> convertConfig cfg
  pushLog "Merging static watches"
  lift $ mergeStaticWatchesS acid watches -- do you even lift bro? how can i cut down on this mess?
  pushLog "Static watches merged"
  where newLogName ctx = ctx { ctxName = "Watch Config Monitor"}
