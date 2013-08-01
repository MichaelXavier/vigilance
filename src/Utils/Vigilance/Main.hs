{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main (main) where

import ClassyPrelude hiding (FilePath)
import Control.Applicative ((<$>))
import Control.Lens
import Control.Monad ((<=<))
import qualified Data.ByteString.Lazy as LBS
import Data.Acid (openLocalStateFrom)
import GHC.IO (FilePath)
import System.Exit (exitFailure)
import Text.InterpolatedString.Perl6 (qc)

import Utils.Vigilance.Config ( loadConfig
                              , configNotifiers )
import Utils.Vigilance.Types
import Utils.Vigilance.Worker (workForeverWithDelayed)
import Utils.Vigilance.Web.Handlers (runServer)
import qualified Utils.Vigilance.Workers.NotificationWorker as NW
import qualified Utils.Vigilance.Workers.SweeperWorker as SW
import Utils.Vigilance.Notifiers.Log ( openLogger
                                     , loggerPutStr
                                     , LogStr(LB)
                                     , Logger) --TODO: extract to logger module

--TODO: more sophisticated option parsing
main :: IO ()
main = do configPath <- (fmap unpack . listToMaybe) <$> getArgs
          maybe noConfig runWithConfigPath configPath

runWithConfigPath :: FilePath -> IO ()
runWithConfigPath = runWithConfig <=< loadConfig

runWithConfig :: Config -> IO ()
runWithConfig cfg = do notifiers <- configNotifiers cfg
                       logger    <- openLogger $ cfg ^. configLogPath
                       let sweeperH  = errorLogger "Sweeper"  logger
                       let notifierH = errorLogger "Notifier" logger
                       acid      <- openLocalStateFrom (cfg ^. configAcidPath) (AppState mempty) 
                       let sweeperWorker  = SW.runWorker acid
                       let notifierWorker = NW.runWorker acid notifiers
                       workForeverWithDelayed sweeperDelay sweeperH sweeperWorker --laziness is gonna fuck me here isn't it?
                       workForeverWithDelayed notifierDelay notifierH notifierWorker
                       runServer cfg

errorLogger :: LBS.ByteString -> Logger -> SomeException -> IO ()
errorLogger ctx logger e = loggerPutStr logger [errMsg]
  where errMsg = LB [qc|Error in {ctx}: {e}|]

sweeperDelay :: Int
sweeperDelay = 5 -- arbitrary

notifierDelay :: Int
notifierDelay = 300 -- arbitrary

noConfig :: IO ()
noConfig = putStrLn "config file argument missing" >> exitFailure
