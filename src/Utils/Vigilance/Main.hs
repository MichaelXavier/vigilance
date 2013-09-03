{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main (main) where

import ClassyPrelude hiding ( FilePath
                            , fromList )
import Control.Concurrent (forkIO)
import Control.Concurrent.Async ( waitAnyCatchCancel
                                , cancel
                                , Async
                                , async )
import Control.Concurrent.STM ( atomically
                              , newBroadcastTChan
                              , TChan
                              , writeTChan
                              , dupTChan )
import Control.Lens
import Control.Monad.Reader ( ask
                            , asks )
import Data.Acid ( AcidState
                 , openLocalStateFrom
                 , createCheckpoint
                 , closeAcidState )
import qualified Data.Configurator as C
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
                              , runInLogCtx
                              , renameLogCtx
                              , vLog
                              , pushLogs
                              , pushLog )
import Utils.Vigilance.TableOps (fromList)
import Utils.Vigilance.Types
import Utils.Vigilance.Utils ( bindM2
                             , newWakeSig
                             , waitForWake
                             , wakeUp )
import Utils.Vigilance.Worker ( workForeverWithDelayed
                              , workForeverWith )
import Utils.Vigilance.Web.Yesod (runServer, WebApp(..))
import qualified Utils.Vigilance.Workers.LoggerWorker as LW
import qualified Utils.Vigilance.Workers.NotificationWorker as NW
import qualified Utils.Vigilance.Workers.NotificationRetryWorker as RW
import qualified Utils.Vigilance.Workers.StaticWatchWorker as WW
import qualified Utils.Vigilance.Workers.SweeperWorker as SW

main :: IO ()
main = do configPath <- (fmap unpack . listToMaybe) <$> getArgs
          maybe noConfig runWithConfigPath configPath

runWithConfigPath :: FilePath -> IO ()
runWithConfigPath path = bindM2 runInMainLogCtx (loadRawConfig path) createLogChan

runInMainLogCtx :: CT.Config -> TChan [LogMessage] -> IO ()
runInMainLogCtx rCfg logChan = do let ctx = LogCtx "Main" logChan
                                  runInLogCtx ctx $ runWithConfig rCfg

runWithConfig :: CT.Config -> LogCtxT IO ()
runWithConfig rCfg = do cfg     <- lift $ convertConfig rCfg
                        lCtx    <- ask
                        logChan <- asks (view ctxChan)

                        (configChanW, configChanR, configChanR') <- lift $ atomically $ do w  <- newBroadcastTChan
                                                                                           r  <- dupTChan w
                                                                                           r' <- dupTChan w
                                                                                           return (w, r, r')

                        let notifiers = configNotifiers cfg
                        acid      <- lift $ openLocalStateFrom (cfg ^. configAcidPath) (AppState (initialState cfg) mempty)
                        quitSig   <- lift newWakeSig

                        let sweeperH       = errorLogger "Sweeper" lCtx
                        let notifierH      = errorLogger "Notifier" lCtx
                        let loggerH        = errorLogger "Logger" lCtx
                        let staticH        = errorLogger "Config Reload" lCtx
                        let retryH         = errorLogger "Retry" lCtx
                        let sweeperWorker  = runInLogCtx lCtx $ SW.runWorker acid
                        let notifierWorker = runInLogCtx lCtx $ NW.runWorker acid notifiers
                        let retryWorker    = runInLogCtx lCtx $ RW.runWorker acid (cfg ^. configMaxRetries) notifiers
                        let loggerWorker   = LW.runWorker logChan cfg configChanR
                        let watchWorker    = runInLogCtx lCtx $ WW.runWorker acid configChanR'
                        let webApp         = WebApp acid cfg logChan

                        vLog "Starting logger" -- TIME PARADOX

                        logger <- lift $ async $ workForeverWith loggerH loggerWorker

                        vLog "Starting sweeper"

                        sweeper <- lift $ async $ workForeverWithDelayed sweeperDelay sweeperH sweeperWorker

                        vLog "Sweeper started"
                        vLog "Starting notifier"

                        nworker <- lift $ async $ workForeverWithDelayed notifierDelay notifierH notifierWorker

                        vLog "Notifier started"

                        vLog "Starting retry worker"

                        rworker <- lift $ async $ workForeverWithDelayed retryDelay retryH retryWorker

                        vLog "Retry worker started"

                        vLog "Starting web server"
                        --TODO: give custom logger to server
                        server <- lift $ async $ runServer webApp

                        static <- lift $ async $ workForeverWith staticH watchWorker

                        let workers = [ server
                                      , sweeper
                                      , nworker
                                      , rworker
                                      , static ]

                        vLog "configuring signal handlers"

                        lift $ do
                          void $ installHandler sigHUP  (Catch $ broadcastCfgReload rCfg configChanW) Nothing
                          void $ installHandler sigINT  (Catch $ wakeUp quitSig ExitSuccess) Nothing
                          void $ installHandler sigTERM (Catch $ wakeUp quitSig ExitSuccess) Nothing

                        vLog "waiting for any process to fail"

                        void . lift . forkIO $ do
                          print . snd =<< waitAnyCatchCancel (logger:workers)
                          wakeUp quitSig (ExitFailure 1)

                        vLog "waiting for quit signal"
                        code <- lift $ waitForWake quitSig
                        lift $ print code

                        cleanUp acid workers code
  where initialState :: Config -> WatchTable
        initialState cfg = fromList $ cfg ^. configWatches

broadcastCfgReload :: CT.Config -> TChan Config -> IO ()
broadcastCfgReload rCfg chan = C.reload rCfg >> broadcast
  where broadcast = atomically . writeTChan chan =<< convertConfig rCfg

errorLogger :: Text -> LogCtx -> SomeException -> IO ()
errorLogger name ctx e =  runInLogCtx ctx $ renameLogCtx name $ pushLog errMsg
  where errMsg = [qc|Error: {e}|] :: Text

sweeperDelay :: Int
sweeperDelay = 5 -- arbitrary

notifierDelay :: Int
notifierDelay = 5 -- arbitrary

retryDelay :: Int
--retryDelay = 30
retryDelay = 10

noConfig :: IO ()
noConfig = putStrLn "config file argument missing" >> exitFailure

cleanUp :: AcidState AppState -> [Async ()] -> ExitCode -> LogCtxT IO ()
cleanUp acid workers code = do pushLogs ["cleaning up", "killing workers"]
                               lift $ mapM_ cancel workers
                               pushLog "creating checkpoint"
                               lift $ createCheckpoint acid
                               pushLog "closing acid"
                               lift $ closeAcidState acid >> exitWith code
