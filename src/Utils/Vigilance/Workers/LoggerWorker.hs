{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils.Vigilance.Workers.LoggerWorker ( runWorker ) where

import Prelude (FilePath)
import ClassyPrelude hiding (FilePath)
import Control.Concurrent.STM (orElse, atomically, STM)
import Control.Concurrent.STM.TChan ( readTChan
                                    , TChan )
import Control.Lens
import Control.Monad ( forever
                     , (<=<) )
import System.IO (openFile
                 , withFile
                 , IOMode(AppendMode))
import System.Log.FastLogger ( Logger
                             , LogStr(..)
                             , mkLogger
                             , rmLogger
                             , toLogStr
                             , loggerDate
                             , loggerPutStr )

import Utils.Vigilance.Types

-- lift into the either monad?
-- liftM Foo (takeTMVar fooTMVar) `orElse` liftM Bar (readTChan barTChan) 
runWorker :: LogChan -> Config -> TChan Config -> IO ()
runWorker q Config { _configLogCfg = logCfg } cfgChan = do
  logger <- openLogger $ logCfg ^. logCfgPath
  logOrReconfigure logger q cfgChan $ logCfg ^. logCfgVerbose

logOrReconfigure :: Logger -> LogChan -> TChan Config -> Bool -> IO ()
logOrReconfigure logger q cfgChan verbose = do
  res <- atomically $ popOrGetCfg q cfgChan
  case res of
    Left msgs -> logMessages logger verbose msgs >> recurse
    Right cfg -> rmLogger logger                 >> runWorker q cfg cfgChan
  where recurse = logOrReconfigure logger q cfgChan verbose

popOrGetCfg :: LogChan -> TChan Config -> STM (Either [LogMessage] Config)
popOrGetCfg q cfgChan = pop `orElse` getCfg
  where pop    = Left <$> readTChan q
        getCfg = Right <$> readTChan cfgChan

logMessages :: Logger -> Bool -> [LogMessage] -> IO ()
logMessages logger verbose = loggerPutStr logger <=< addTime logger . map toLogStr . filter keep
  where keep (LogMessage _)        = True
        keep (VerboseLogMessage _) = verbose

openLogger :: FilePath -> IO Logger
openLogger path = mkLogger flushEveryLine =<< openFile path AppendMode
  where flushEveryLine = True

addTime :: Logger -> [LogStr] -> IO [LogStr]
addTime logger = mapM fmt 
  where fmt str = do date <- loggerDate logger
                     return . LB $ mconcat ["[", date ,"] ", toBS str]
        toBS (LB bs) = bs
        toBS (LS s)  = encodeUtf8 . pack $ s
