{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main (main) where

import ClassyPrelude hiding (FilePath)
import Control.Applicative ((<$>))
import Control.Lens
import Control.Monad ((<=<))
import Control.Monad.Reader (runReaderT)
import qualified Data.ByteString.Lazy as LBS
import Data.Acid (openLocalStateFrom)
import GHC.IO (FilePath)
import System.Exit (exitFailure)
import System.Log.FastLogger ( LogStr(LB) ) --todo: reexport from types
import Text.InterpolatedString.Perl6 (qc)

import Utils.Vigilance.Config ( loadConfig
                              , configNotifiers )
import Utils.Vigilance.Logger ( createLogChan
                              , pushLogs )
import Utils.Vigilance.Types
import Utils.Vigilance.Worker (workForeverWithDelayed)
import Utils.Vigilance.Web.Handlers (runServer)
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

                       let sweeperH       = errorLogger "Sweeper"  logChan
                       let notifierH      = errorLogger "Notifier" logChan
                       let sweeperWorker  = SW.runWorker acid
                       let notifierWorker = NW.runWorker acid notifiers

                       workForeverWithDelayed sweeperDelay sweeperH sweeperWorker --laziness is gonna fuck me here isn't it?
                       workForeverWithDelayed notifierDelay notifierH notifierWorker

                       --TODO: give custom logger to server
                       runServer cfg

errorLogger :: LBS.ByteString -> LogChan -> SomeException -> IO ()
errorLogger ctx logChan e =  pushLogs logChan [errMsg]
  where errMsg = LB [qc|Error in {ctx}: {e}|]

sweeperDelay :: Int
sweeperDelay = 5 -- arbitrary

notifierDelay :: Int
notifierDelay = 300 -- arbitrary

noConfig :: IO ()
noConfig = putStrLn "config file argument missing" >> exitFailure
