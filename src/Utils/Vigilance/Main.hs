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
import Control.Concurrent.STM ( atomically
                              , newBroadcastTChan
                              , TChan
                              , writeTChan
                              , dupTChan )
import Control.Lens
import Control.Monad ( (<=<)
                     , void )
import Control.Monad.Reader ( runReaderT
                            , ask
                            , asks)
import Control.Monad.Trans (lift)
import qualified Data.ByteString.Lazy as LBS
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

main :: IO ()
main = do configPath <- (fmap unpack . listToMaybe) <$> getArgs
          maybe noConfig runWithConfigPath configPath

runWithConfigPath :: FilePath -> IO ()
runWithConfigPath path = bindM2 runInMainLogCtx (loadRawConfig path) createLogChan

runInMainLogCtx rCfg logChan = do let ctx = LogCtx "Main" logChan
                                  runInLogCtx ctx $ runWithConfig rCfg

runWithConfig :: CT.Config -> LogCtxT IO ()
runWithConfig rCfg = do cfg       <- lift $ convertConfig rCfg
                        logCtx    <- ask
                        logChan   <- asks (view ctxChan)

                        (configChanW, configChanR, configChanR') <- lift $ atomically $ do w  <- newBroadcastTChan
                                                                                           r  <- dupTChan w
                                                                                           r' <- dupTChan w
                                                                                           return (w, r, r')

                        let notifiers = configNotifiers cfg
                        acid      <- lift $ openLocalStateFrom (cfg ^. configAcidPath) (AppState $ initialState cfg)
                        quitSig   <- lift $ (newWakeSig :: IO (WakeSig ExitCode))

                        let sweeperH       = errorLogger "Sweeper" logCtx
                        let notifierH      = errorLogger "Notifier" logCtx
                        let loggerH        = errorLogger "Logger" logCtx
                        let staticH        = errorLogger "Config Reload" logCtx
                        let sweeperWorker  = runInLogCtx logCtx $ SW.runWorker acid
                        let notifierWorker = runInLogCtx logCtx $ NW.runWorker acid notifiers
                        let logCfg         = cfg ^. configLogCfg
                        let loggerWorker   = LW.runWorker logChan cfg configChanR
                        let watchWorker    = runInLogCtx logCtx $ WW.runWorker acid configChanR'
                        let webApp         = WebApp acid cfg logChan

                        vLog "Starting logger" -- TIME PARADOX

                        logger <- lift $ async $ workForeverWith loggerH loggerWorker

                        vLog "Starting sweeper"

                        sweeper <- lift $ async $ workForeverWithDelayed sweeperDelay sweeperH sweeperWorker

                        vLog "Sweeper started"
                        vLog "Starting notifier"

                        notifier <- lift $ async $ workForeverWithDelayed notifierDelay notifierH notifierWorker

                        vLog "Notifier started"

                        vLog "Starting web server"
                        --TODO: give custom logger to server
                        server <- lift $ async $ runServer webApp

                        static <- lift $ async $ workForeverWith staticH watchWorker

                        let workers = [ server
                                      , sweeper
                                      , notifier
                                      , static ]

                        vLog "configuring signal handlers"

                        lift $ do
                          installHandler sigHUP  (Catch $ broadcastCfgReload rCfg configChanW) Nothing
                          installHandler sigINT  (Catch $ wakeUp quitSig ExitSuccess) Nothing
                          installHandler sigTERM (Catch $ wakeUp quitSig ExitSuccess) Nothing

                        vLog "waiting for any process to fail"

                        lift . forkIO $ do
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
--notifierDelay = 300 -- arbitrary
notifierDelay = 5 -- arbitrary

noConfig :: IO ()
noConfig = putStrLn "config file argument missing" >> exitFailure

cleanUp :: AcidState AppState -> [Async ()] -> ExitCode -> LogCtxT IO ()
cleanUp acid workers code = do pushLogs ["cleaning up", "killing workers"]
                               lift $ mapM_ cancel workers
                               pushLog "creating checkpoint"
                               lift $ createCheckpoint acid
                               pushLog "closing acid"
                               lift $ closeAcidState acid >> exitWith code
