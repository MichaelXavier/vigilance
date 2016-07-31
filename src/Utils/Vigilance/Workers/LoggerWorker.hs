{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils.Vigilance.Workers.LoggerWorker (runWorker) where

import Prelude (FilePath)
import ClassyPrelude hiding (FilePath, (<>))
import Control.Concurrent.STM (orElse)
import Control.Lens
import Data.Monoid ((<>))
import System.Log.FastLogger

import Utils.Vigilance.Types
import Utils.Vigilance.Utils (expandHome)

-- lift into the either monad?
-- liftM Foo (takeTMVar fooTMVar) `orElse` liftM Bar (readTChan barTChan)
runWorker :: LogChan -> Config -> TChan Config -> IO ()
runWorker q Config { _configLogCfg = cfg } cfgChan = do
  logger <- openLogger $ cfg ^. logCfgPath
  logOrReconfigure logger q cfgChan $ cfg ^. logCfgVerbose

logOrReconfigure :: LoggerSet -> LogChan -> TChan Config -> Bool -> IO ()
logOrReconfigure logger q cfgChan verbose = do
  res <- atomically $ popOrGetCfg q cfgChan
  case res of
    Left msgs -> logMessages logger verbose msgs >> recurse
    Right cfg -> rmLoggerSet logger              >> runWorker q cfg cfgChan
  where recurse = logOrReconfigure logger q cfgChan verbose

popOrGetCfg :: LogChan -> TChan Config -> STM (Either [LogMessage] Config)
popOrGetCfg q cfgChan = pop `orElse` getCfg
  where pop    = Left <$> readTChan q
        getCfg = Right <$> readTChan cfgChan

logMessages :: LoggerSet -> Bool -> [LogMessage] -> IO ()
logMessages logger verbose = mapM_ (pushLogStrLn logger) <=< addTime logger . map toLogStr . filter keep
  where keep (LogMessage _)        = True
        keep (VerboseLogMessage _) = verbose

openLogger :: FilePath -> IO LoggerSet
openLogger path = newFileLoggerSet 60 =<< expandHome path


-- | TODO: this doesn't use time formatting caching any more.  makes things slow, but shouldn't
-- break anything.
addTime :: LoggerSet -> [LogStr] -> IO [LogStr]
addTime _logger = mapM fmt
  where fmt str = do date <- formatTime defaultTimeLocale "%d%b%Y:%T %z" <$> getCurrentTime
                     return $ toLogStr (mconcat ["[", date ,"] "]) <> str
