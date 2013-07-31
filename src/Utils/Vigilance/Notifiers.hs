module Utils.Vigilance.Notifiers (notifyAll) where

import Control.Monad (mapM_)
import Utils.Vigilance.Types

notifyAll :: [EWatch] -> [Notifier] -> IO ()
notifyAll ws = mapM_ ($ ws)
