{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Utils.Vigilance.Notifiers.Log ( notify
                                     , openLogger) where

import ClassyPrelude hiding (FilePath)
import Control.Applicative ( (<$>)
                           , (<*>)
                           , pure )
import Control.Lens
import GHC.IO (FilePath)
import Data.Monoid (mconcat)
import System.IO (openFile
                 , IOMode(AppendMode))
import System.Log.FastLogger ( Logger
                             , mkLogger
                             , loggerPutStr
                             , LogStr(LB))

import Utils.Vigilance.Types

-- maybe error return type
notify :: Logger -> Notifier
notify logger watches = loggerPutStr logger formattedWatches
  where formattedWatches = map format watches
        format w         = LB $ mconcat ["Watch "
                                        , w ^. watchName . to encodeUtf8
                                        , " failed to check in." ]

openLogger :: FilePath -> IO Logger
openLogger path = mkLogger flushEveryLine =<< openFile path AppendMode
  where flushEveryLine = True
