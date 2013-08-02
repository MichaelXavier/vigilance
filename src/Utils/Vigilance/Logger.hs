module Utils.Vigilance.Logger ( createLogChan
                              , pushLogs ) where

import Control.Concurrent.Chan ( writeChan
                               , newChan )
import System.Log.FastLogger ( LogStr ) --todo: reexport from types

import Utils.Vigilance.Types

createLogChan :: IO LogChan
createLogChan = newChan

pushLogs :: LogChan -> [LogStr] -> IO ()
pushLogs = writeChan
