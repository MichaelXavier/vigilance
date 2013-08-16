{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils.Vigilance.Workers.LoggerWorker ( runWorker ) where

import Prelude (FilePath)
import ClassyPrelude hiding (FilePath)
import Control.Concurrent.STM (orElse, atomically, STM)
import Control.Concurrent.STM.TChan (readTChan)
import Control.Concurrent.STM.TMVar (TMVar, takeTMVar)
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
runWorker :: LogChan -> LogCfg -> TMVar LogCfg -> IO ()
runWorker q cfg cfgV = do
  logger <- openLogger $ cfg ^. logCfgPath
  logOrReconfigure logger q cfgV $ cfg ^. logCfgVerbose

logOrReconfigure :: Logger -> LogChan -> TMVar LogCfg -> Bool -> IO ()
logOrReconfigure logger q cfgV verbose = do
  res <- atomically $ popOrReconfigure q cfgV
  case res of
    Left msgs -> logMessages logger verbose msgs >> recurse
    Right cfg -> rmLogger logger                 >> runWorker q cfg cfgV
  where recurse = logOrReconfigure logger q cfgV verbose

popOrReconfigure :: LogChan -> TMVar LogCfg -> STM (Either [LogMessage] LogCfg)
popOrReconfigure q cfgV = (Left <$> (readTChan q)) `orElse` (Right <$> (takeTMVar cfgV))

--TODO: actually use verbosity
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
