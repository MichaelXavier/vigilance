{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main (main) where

import ClassyPrelude hiding ( FilePath
                            , fromList )
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Concurrent.Async ( waitAnyCatchCancel
                                , cancel
                                , Async
                                , async )
import Control.Lens
import Control.Monad ((<=<))
import Control.Monad.Reader (runReaderT)
import qualified Data.ByteString.Lazy as LBS
import Data.Acid ( AcidState
                 , openLocalStateFrom
                 , createCheckpoint
                 , closeAcidState )
import qualified Data.Configurator.Types as CT
import GHC.IO (FilePath)
import System.Exit ( exitFailure
                   , ExitCode(..)
                   , exitWith )
import System.Posix.Signals ( installHandler
                            , sigHUP
                            , sigINT
                            , sigTERM
                            , Handler(Catch) )
import Text.InterpolatedString.Perl6 (qc)

import Utils.Vigilance.Config ( loadRawConfig
                              , convertConfig
                              , configNotifiers )
import Utils.Vigilance.Logger ( createLogChan
                              , pushLogs
                              , pushLog )
import Utils.Vigilance.TableOps (fromList)
import Utils.Vigilance.Types
import Utils.Vigilance.Utils ( newWakeSig
                             , WakeSig
                             , waitForWake
                             , wakeUp )
import Utils.Vigilance.Worker ( workForeverWithDelayed
                              , workForeverWith )
import Utils.Vigilance.Web.Yesod (runServer, WebApp(..))
import qualified Utils.Vigilance.Workers.LoggerWorker as LW
import qualified Utils.Vigilance.Workers.NotificationWorker as NW
import qualified Utils.Vigilance.Workers.StaticWatchWorker as WW
import qualified Utils.Vigilance.Workers.SweeperWorker as SW

--TODO: more sophisticated option parsing
main :: IO ()
main = do configPath <- (fmap unpack . listToMaybe) <$> getArgs
          maybe noConfig runWithConfigPath configPath

runWithConfigPath :: FilePath -> IO ()
runWithConfigPath = runWithConfig <=< loadRawConfig

runWithConfig :: CT.Config -> IO ()
runWithConfig rCfg = do cfg       <- convertConfig rCfg
                        logChan   <- createLogChan
                        notifiers <- runReaderT (configNotifiers cfg) logChan
                        acid      <- openLocalStateFrom (cfg ^. configAcidPath) (AppState $ initialState cfg)
                        wakeSig   <- newWakeSig :: IO (WakeSig ())
                        quitSig   <- newWakeSig :: IO (WakeSig ExitCode)

                        let log = pushLog logChan :: LBS.ByteString -> IO ()
                        let sweeperH       = errorLogger "Sweeper"  logChan
                        let notifierH      = errorLogger "Notifier" logChan
                        let loggerH        = errorLogger "Logger" logChan
                        let staticH        = errorLogger "Config Reload" logChan
                        let sweeperWorker  = SW.runWorker acid
                        let notifierWorker = NW.runWorker acid notifiers
                        let loggerWorker   = LW.runWorker (cfg ^. configLogPath) logChan
                        let watchWorker    = WW.runWorker acid logChan rCfg wakeSig
                        let webApp         = WebApp acid cfg logChan

                        log "Starting logger" -- TIME PARADOX

                        logger <- async $ workForeverWithDelayed sweeperDelay loggerH loggerWorker

                        log "Starting sweeper"

                        sweeper <- async $ workForeverWithDelayed sweeperDelay sweeperH sweeperWorker

                        log "Sweeper started"
                        log "Starting notifier"

                        notifier <- async $ workForeverWith notifierH notifierWorker

                        log "Notifier started"

                        log "Starting web server"
                        --TODO: give custom logger to server
                        server <- async $ runServer webApp

                        static <- async $ workForeverWithDelayed notifierDelay staticH watchWorker

                        let workers = [ server
                                      , sweeper
                                      , notifier
                                      , static ]

                        log "configuring signal handlers"
                        installHandler sigHUP  (Catch $ wakeUp wakeSig ()) Nothing
                        --FIXME: instead of cleanup here, use a tmvar that funnels into 1 blocking call
                        installHandler sigINT  (Catch $ wakeUp quitSig ExitSuccess) Nothing
                        installHandler sigTERM (Catch $ wakeUp quitSig ExitSuccess) Nothing

                        log "waiting for any process to fail"
                        forkIO $ waitAnyCatchCancel (logger:workers) >> wakeUp quitSig (ExitFailure 1)

                        log "waiting for quit signal"
                        code <- waitForWake quitSig

                        cleanUp acid logChan workers code
  where initialState :: Config -> WatchTable
        initialState cfg = fromList $ cfg ^. configWatches

errorLogger :: LBS.ByteString -> LogChan -> SomeException -> IO ()
errorLogger ctx logChan e =  pushLog logChan errMsg
  where errMsg = [qc|Error in {ctx}: {e}|] :: LBS.ByteString

sweeperDelay :: Int
sweeperDelay = 5 -- arbitrary

notifierDelay :: Int
notifierDelay = 300 -- arbitrary

noConfig :: IO ()
noConfig = putStrLn "config file argument missing" >> exitFailure

cleanUp :: AcidState AppState -> LogChan -> [Async ()] -> ExitCode -> IO ()
cleanUp acid logChan workers code = do logs ["cleaning up", "killing workers"]
                                       mapM_ cancel workers
                                       log "creating checkpoint"
                                       createCheckpoint acid
                                       log "closing acid"
                                       closeAcidState acid
                                       exitWith code
  where log :: Text -> IO ()
        log  = pushLog logChan
        logs :: [Text] -> IO ()
        logs = pushLogs logChan
                
