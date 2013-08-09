{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils.Vigilance.Workers.LoggerWorker ( runWorker ) where

import Prelude (FilePath)
import ClassyPrelude hiding (FilePath)
import Control.Concurrent.Chan ( readChan )
import Control.Monad ( forever
                     , (<=<) )
import System.IO (openFile
                 , withFile
                 , IOMode(AppendMode))
import System.Log.FastLogger ( Logger
                             , LogStr(..)
                             , mkLogger
                             , loggerDate
                             , loggerPutStr )

import Utils.Vigilance.Types

runWorker :: FilePath -> LogChan -> IO ()
runWorker path q = do logger <- openLogger path
                      let logMessages' = logMessages logger q
                      forever logMessages'

logMessages :: Logger -> LogChan -> IO ()
logMessages logger = loggerPutStr logger <=< addTime logger <=< readChan

openLogger :: FilePath -> IO Logger
openLogger path = mkLogger flushEveryLine =<< openFile path AppendMode
  where flushEveryLine = True

addTime :: Logger -> [LogStr] -> IO [LogStr]
addTime logger = mapM fmt 
  where fmt str = do date <- loggerDate logger
                     return . LB $ mconcat ["[", date ,"] ", toBS str]
        toBS (LB bs) = bs
        toBS (LS s)  = encodeUtf8 . pack $ s
