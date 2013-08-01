module Utils.Vigilance.Config (configNotifiers) where

import Control.Applicative ((<$>))
import Control.Lens
import Data.Maybe (catMaybes)
import qualified Utils.Vigilance.Notifiers.Email as E
import qualified Utils.Vigilance.Notifiers.Log   as L
import Utils.Vigilance.Types

configNotifiers :: Config -> IO [Notifier]
configNotifiers cfg = do logger      <- L.openLogger $ cfg ^. configLogPath
                         let logNotifier    = L.notify logger
                         let mEmailNotifier = E.notify . E.EmailContext <$> cfg ^. configFromEmail
                         return $ catMaybes [Just logNotifier, mEmailNotifier]
