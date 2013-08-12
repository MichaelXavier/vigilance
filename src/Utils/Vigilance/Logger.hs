{-# LANGUAGE OverloadedStrings #-}
module Utils.Vigilance.Logger ( createLogChan
                              , runInLogCtx
                              , pushLog
                              , pushLogs ) where

import Data.Monoid ( Monoid
                   , mconcat)
import Data.Text (Text)
import Control.Concurrent.Chan ( writeChan
                               , newChan )
import Control.Monad.Reader (runReaderT, asks)
import Control.Monad.Trans (lift)
import System.Log.FastLogger ( ToLogStr(..) ) --todo: reexport from types

import Utils.Vigilance.Types

createLogChan :: IO LogChan
createLogChan = newChan

-- why u no date format :(
-- why must i add newlines you dick?
pushLogs :: [Text] -> LogCtxT IO ()
pushLogs ls = do n       <- asks ctxName
                 logChan <- asks ctxChan
                 lift $ writeChan logChan $ map (fmt n) ls
  where fmt n s = toLogStr $ mconcat [" [", n, "] ", s, "\n"]

pushLog :: Text -> LogCtxT IO ()
pushLog = pushLogs . return

runInLogCtx :: LogCtx -> LogCtxT m a -> m a
runInLogCtx = flip runReaderT
