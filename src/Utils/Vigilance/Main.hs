{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main (main) where

import ClassyPrelude hiding (FilePath)
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Concurrent.Async ( waitAnyCatchCancel
                                , async )
import Control.Lens
import Control.Monad ((<=<))
import Control.Monad.Reader (runReaderT)
import qualified Data.ByteString.Lazy as LBS
import Data.Acid ( openLocalStateFrom
                 , closeAcidState )
import GHC.IO (FilePath)
import System.Exit (exitFailure)
import Text.InterpolatedString.Perl6 (qc)

import Utils.Vigilance.Config ( loadConfig
                              , configNotifiers )
import Utils.Vigilance.Logger ( createLogChan
                              , pushLog )
import Utils.Vigilance.Types
import Utils.Vigilance.Worker (workForeverWithDelayed)
import Utils.Vigilance.Web.Yesod (runServer, WebApp(..))
import qualified Utils.Vigilance.Workers.LoggerWorker as LW
import qualified Utils.Vigilance.Workers.NotificationWorker as NW
import qualified Utils.Vigilance.Workers.SweeperWorker as SW

--TODO: more sophisticated option parsing
main :: IO ()
main = do configPath <- (fmap unpack . listToMaybe) <$> getArgs
          maybe noConfig runWithConfigPath configPath

runWithConfigPath :: FilePath -> IO ()
runWithConfigPath = runWithConfig <=< loadConfig

runWithConfig :: Config -> IO ()
runWithConfig cfg = do logChan   <- createLogChan
                       notifiers <- runReaderT (configNotifiers cfg) logChan
                       acid      <- openLocalStateFrom (cfg ^. configAcidPath) (AppState mempty)

                       let log = pushLog logChan :: LBS.ByteString -> IO ()
                       let sweeperH       = errorLogger "Sweeper"  logChan
                       let notifierH      = errorLogger "Notifier" logChan
                       let loggerH        = errorLogger "Logger" logChan
                       let sweeperWorker  = SW.runWorker acid
                       let notifierWorker = NW.runWorker acid notifiers
                       let loggerWorker   = LW.runWorker (cfg ^. configLogPath) logChan
                       let webApp         = WebApp acid cfg logChan

                       log "Starting logger" -- TIME PARADOX

                       logger <- async $ workForeverWithDelayed sweeperDelay loggerH loggerWorker

                       log "Starting sweeper"

                       sweeper <- async $ workForeverWithDelayed sweeperDelay sweeperH sweeperWorker

                       log "Sweeper started"
                       log "Starting notifier"

                       notifier <- async $ workForeverWithDelayed notifierDelay notifierH notifierWorker

                       log "Notifier started"

                       log "Starting web server"
                       --TODO: give custom logger to server
                       server <- async $ runServer webApp

                       log "waiting for someone to shit the bed"
                       result <- snd <$> waitAnyCatchCancel [logger, server, sweeper, notifier]
                       print result --TODO: better exit handling

                       log "closing acid state"
                       closeAcidState acid

errorLogger :: LBS.ByteString -> LogChan -> SomeException -> IO ()
errorLogger ctx logChan e =  pushLog logChan errMsg
  where errMsg = [qc|Error in {ctx}: {e}|] :: LBS.ByteString

sweeperDelay :: Int
sweeperDelay = 5 -- arbitrary

notifierDelay :: Int
notifierDelay = 300 -- arbitrary

noConfig :: IO ()
noConfig = putStrLn "config file argument missing" >> exitFailure
