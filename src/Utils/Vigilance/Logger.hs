{-# LANGUAGE OverloadedStrings #-}
module Utils.Vigilance.Logger ( createLogChan
                              , runInLogCtx
                              , renameLogCtx
                              , pushLog
                              , vLog
                              , pushLogs ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan ( writeTChan
                                    , newTChan )
import Control.Lens
import Control.Monad (when)
import Control.Monad.Reader ( runReaderT
                            , asks
                            , withReaderT )
import Control.Monad.Trans (lift)
import Data.Monoid ( Monoid
                   , mconcat)
import Data.Text (Text)
import System.Log.FastLogger ( ToLogStr(..) ) --todo: reexport from types

import Utils.Vigilance.Types

createLogChan :: IO LogChan
createLogChan = atomically $ newTChan

pushLogs :: [Text] -> LogCtxT IO ()
pushLogs = pushLogs' . map LogMessage

vLogs :: [Text] -> LogCtxT IO ()
vLogs = pushLogs' . map VerboseLogMessage

pushLog :: Text -> LogCtxT IO ()
pushLog = pushLogs' . return . LogMessage

vLog :: Text -> LogCtxT IO ()
vLog = pushLogs' . return . VerboseLogMessage

pushLogs' :: [LogMessage] -> LogCtxT IO ()
pushLogs' ls = do n        <- asks (view ctxName)
                  logChan  <- asks (view ctxChan)
                  lift $ atomically $ writeTChan logChan $ map (fmt n) ls
  where fmt n (LogMessage s)        = LogMessage $ fmt' n s
        fmt n (VerboseLogMessage s) = VerboseLogMessage $ fmt' n s
        fmt' n s                    = mconcat ["[", n, "] ", s, "\n"] -- why must i add newlines you dick?


runInLogCtx :: LogCtx -> LogCtxT m a -> m a
runInLogCtx = flip runReaderT

renameLogCtx :: Text -> LogCtxT m a -> LogCtxT m a
renameLogCtx newName = withReaderT rename
  where rename ctx = ctx & ctxName .~ newName
