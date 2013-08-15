{-# LANGUAGE OverloadedStrings #-}
module Utils.Vigilance.Logger ( createLogChan
                              , runInLogCtx
                              , renameLogCtx
                              , pushLog
                              , vLog
                              , pushLogs ) where

import Control.Concurrent.Chan ( writeChan
                               , newChan )
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
createLogChan = newChan

-- why u no date format :(
-- why must i add newlines you dick?
pushLogs :: [Text] -> LogCtxT IO ()
pushLogs ls = do n       <- asks (view ctxName)
                 logChan <- asks (view ctxChan)
                 lift $ writeChan logChan $ map (fmt n) ls
  where fmt n s = toLogStr $ mconcat ["[", n, "] ", s, "\n"]

pushLog :: Text -> LogCtxT IO ()
pushLog = pushLogs . return

vLog :: Text -> LogCtxT IO ()
vLog l = do verbose <- asks (view ctxVerbose)
            when verbose $ pushLog l

runInLogCtx :: LogCtx -> LogCtxT m a -> m a
runInLogCtx = flip runReaderT

renameLogCtx :: Text -> LogCtxT m a -> LogCtxT m a
renameLogCtx newName = withReaderT rename
  where rename ctx = ctx & ctxName .~ newName
