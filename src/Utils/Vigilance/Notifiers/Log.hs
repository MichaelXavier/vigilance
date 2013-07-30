{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Utils.Vigilance.Notifiers.Log (notify) where

import ClassyPrelude
import Control.Applicative ( (<$>)
                           , (<*>)
                           , pure)
import Control.Lens
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader
import Data.Monoid (mconcat)
import System.Log.FastLogger (Logger, loggerPutStr, LogStr(LB))

import Utils.Vigilance.Types

-- maybe error return type
notify :: [EWatch] -> ReaderT Logger IO ()
notify watches = do logger <- ask
                    lift $ loggerPutStr logger formattedWatches
  where formattedWatches = map format watches
        format w         = LB $ mconcat ["Watch "
                                        , w ^. watchName . to encodeUtf8
                                        , " failed to check in." ]
