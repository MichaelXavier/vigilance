{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils.Vigilance.Utils ( watchIntervalSeconds
                             , WakeSig
                             , newWakeSig
                             , wakeUp
                             , waitForWake
                             , concatMapM
                             , tryReadTChan
                             , expandHome
                             , bindM3
                             , bindM2) where

import ClassyPrelude hiding (FilePath, tryReadTChan)
import Prelude (FilePath)

import Control.Monad.STM (orElse)
import qualified Data.Text as T
import System.Directory (getHomeDirectory)
import Utils.Vigilance.Types

watchIntervalSeconds :: WatchInterval -> Integer
watchIntervalSeconds (Every n Seconds) = n
watchIntervalSeconds (Every n Minutes) = n * 60
watchIntervalSeconds (Every n Hours)   = n * 60 * 60
watchIntervalSeconds (Every n Days)    = n * 60 * 60 * 24
watchIntervalSeconds (Every n Weeks)   = n * 60 * 60 * 24 * 7
watchIntervalSeconds (Every n Years)   = n * 60 * 60 * 24 * 365


concatMapM :: (Monad m, Applicative m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = (concat <$>) . mapM f

bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f m1 m2  = join $ liftM2 f m1 m2

bindM3 :: Monad m => (a -> b -> c -> m d) -> m a -> m b -> m c -> m d
bindM3 f m1 m2 m3  = join $ liftM3 f m1 m2 m3

type WakeSig a = TMVar a

newWakeSig :: IO (WakeSig a)
newWakeSig = newEmptyTMVarIO

waitForWake :: WakeSig a -> IO a
waitForWake = atomically . takeTMVar

wakeUp :: WakeSig a -> a -> IO ()
wakeUp sig = atomically . putTMVar sig

tryReadTChan :: TChan a -> STM (Maybe a)
tryReadTChan c = (Just <$> readTChan c) `orElse` return Nothing

expandHome :: FilePath -> IO FilePath
expandHome path = do home <- pack <$> getHomeDirectory
                     return . unpack . T.replace homeVar home . pack $ path
  where homeVar = "$(HOME)"
