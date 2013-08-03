{-# LANGUAGE OverloadedStrings #-}
module Utils.Vigilance.Logger ( createLogChan
                              , pushLog
                              , pushLogs ) where

import Data.Monoid ( Monoid
                   , (<>))
import Data.String (IsString)
import Control.Concurrent.Chan ( writeChan
                               , newChan )
import System.Log.FastLogger ( ToLogStr(..) ) --todo: reexport from types

import Utils.Vigilance.Types

createLogChan :: IO LogChan
createLogChan = newChan

-- why u no date format :(
-- why must i add newlines you dick?
pushLogs :: (ToLogStr a, Monoid a, IsString a) => LogChan -> [a] -> IO ()
pushLogs logChan = writeChan logChan . map fmt
  where fmt = toLogStr . (<> "\n")

pushLog :: (ToLogStr a, Monoid a, IsString a) => LogChan -> a -> IO ()
pushLog logChan = pushLogs logChan . return
