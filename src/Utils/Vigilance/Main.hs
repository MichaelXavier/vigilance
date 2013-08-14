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
import Control.Monad ( (<=<)
                     , void )
import Control.Monad.Reader ( runReaderT
                            , withReaderT
                            , ask
                            , asks)
import Control.Monad.Trans (lift)
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
                              , runInLogCtx
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

--TODO: more sophisticated option parsing
main :: IO ()
main = do configPath <- (fmap unpack . listToMaybe) <$> getArgs
          maybe noConfig runWithConfigPath configPath

runWithConfigPath :: FilePath -> IO ()
runWithConfigPath path = bindM2 runInMainLogCtx (loadRawConfig path) createLogChan

runInMainLogCtx rCfg logChan = runInLogCtx ctx $ runWithConfig rCfg
  where ctx = LogCtx "Main" logChan

runWithConfig :: CT.Config -> LogCtxT IO ()
runWithConfig rCfg = do cfg       <- lift $ convertConfig rCfg
                        logCtx    <- ask
                        logChan   <- asks ctxChan --TODO: rewrite others
                        let notifiers = configNotifiers cfg
                        acid      <- lift $ openLocalStateFrom (cfg ^. configAcidPath) (AppState $ initialState cfg)
                        wakeSig   <- lift $ (newWakeSig :: IO (WakeSig ()))
                        quitSig   <- lift $ (newWakeSig :: IO (WakeSig ExitCode))

                        let sweeperH       = errorLogger "Sweeper" logCtx
                        let notifierH      = errorLogger "Notifier" logCtx
                        let loggerH        = errorLogger "Logger" logCtx
                        let staticH        = errorLogger "Config Reload" logCtx
                        let sweeperWorker  = SW.runWorker acid
                        let notifierWorker = runInLogCtx logCtx $ NW.runWorker acid notifiers
                        let loggerWorker   = LW.runWorker (cfg ^. configLogPath) logChan
                        let watchWorker    = runInLogCtx logCtx $ WW.runWorker acid rCfg wakeSig
                        let webApp         = WebApp acid cfg logChan

                        pushLog "Starting logger" -- TIME PARADOX

                        logger <- lift $ async $ workForeverWithDelayed sweeperDelay loggerH loggerWorker

                        pushLog "Starting sweeper"

                        sweeper <- lift $ async $ workForeverWithDelayed sweeperDelay sweeperH sweeperWorker

                        pushLog "Sweeper started"
                        pushLog "Starting notifier"

                        notifier <- lift $ async $ workForeverWithDelayed notifierDelay notifierH notifierWorker

                        pushLog "Notifier started"

                        pushLog "Starting web server"
                        --TODO: give custom logger to server
                        server <- lift $ async $ runServer webApp

                        static <- lift $ async $ workForeverWith staticH watchWorker

                        let workers = [ server
                                      , sweeper
                                      , notifier
                                      , static ]

                        pushLog "configuring signal handlers"

                        lift $ do
                          installHandler sigHUP  (Catch $ wakeUp wakeSig ()) Nothing
                          installHandler sigINT  (Catch $ wakeUp quitSig ExitSuccess) Nothing
                          installHandler sigTERM (Catch $ wakeUp quitSig ExitSuccess) Nothing

                        pushLog "waiting for any process to fail"

                        lift . forkIO $ do
                          print . snd =<< waitAnyCatchCancel (logger:workers)
                          wakeUp quitSig (ExitFailure 1)

                        pushLog "waiting for quit signal"
                        code <- lift $ waitForWake quitSig
                        lift $ print code

                        cleanUp acid workers code
  where initialState :: Config -> WatchTable
        initialState cfg = fromList $ cfg ^. configWatches

errorLogger :: Text -> LogCtx -> SomeException -> IO ()
errorLogger name ctx e =  runInLogCtx ctx' $ pushLog errMsg
  where errMsg = [qc|Error: {e}|] :: Text
        ctx'   = ctx { ctxName = name }

sweeperDelay :: Int
sweeperDelay = 5 -- arbitrary

notifierDelay :: Int
notifierDelay = 300 -- arbitrary

noConfig :: IO ()
noConfig = putStrLn "config file argument missing" >> exitFailure

cleanUp :: AcidState AppState -> [Async ()] -> ExitCode -> LogCtxT IO ()
cleanUp acid workers code = do pushLogs ["cleaning up", "killing workers"]
                               lift $ mapM_ cancel workers
                               pushLog "creating checkpoint"
                               lift $ createCheckpoint acid
                               pushLog "closing acid"
                               lift $ closeAcidState acid >> exitWith code
