{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Prelude (FilePath)
import ClassyPrelude hiding (FilePath, (<>))
import Data.String.Conversions (cs)
import Options.Applicative
import System.Exit ( exitSuccess
                   , exitFailure)

import Utils.Vigilance.Client.Client
import Utils.Vigilance.Client.Config
import Utils.Vigilance.Types

main :: IO ()
main = runOptions =<< execParser opts

runOptions :: Options -> IO ()
runOptions Options {..} = runReaderT (runCommand optCommand) =<< readClientConfig configPath

runCommand :: Command -> ClientCtxT IO ()
runCommand List        = withErrorHandling displayList                  getList
runCommand (Pause n)   = withErrorHandling doNothing                  $ pause n
runCommand (UnPause n) = withErrorHandling doNothing                  $ unPause n
runCommand (CheckIn n) = withErrorHandling doNothing                  $ checkIn n
runCommand (Info n)    = withErrorHandling displayWatchInfo           $ getInfo n
runCommand (Test n)    = withErrorHandling handleTestResults          $ test n

handleTestResults :: [FailedNotification] -> IO ()
handleTestResults failures = do displayFailedNotifications failures
                                if null failures
                                  then exitSuccess
                                  else exitFailure

doNothing :: a -> IO ()
doNothing = const $ return ()

withErrorHandling :: (a -> IO ()) -> ClientCtxT IO (Either VError a) -> ClientCtxT IO ()
withErrorHandling display act = either (lift . displayError) (lift . display) =<< act

displayError :: VError -> IO ()
displayError e = putStrLn (message e) >> exitFailure
  where message NotFound           = "Watch not found"
        message (ParseError msg)   = "Parse error: " <> msg
        message (StatusError code) = "Server returned error status: " <> cs (show code)

opts :: ParserInfo Options
opts = info (helper <*> optionsParser) banner
  where banner = fullDesc <>
                 header "vigilance - tool for managing vigilance watches locally or remotely."

optionsParser :: Parser Options
optionsParser = Options <$> subparser commandParser
               <*> pathParser

pathParser :: Parser FilePath
pathParser = parser <|> pure defaultConfigPath
  where parser = strOption $ long "config"  <>
                             short 'c'      <>
                             metavar "FILE" <>
                             help (mconcat ["Config file. Defaults to ", pack vigilanceDir])


commandParser :: Mod CommandFields Command
commandParser = listParser    <>
                pauseParser   <>
                unPauseParser <>
                checkInParser <>
                infoParser    <>
                testParser
  where listParser    = command "list"   $
                        info (pure List) $
                        progDesc "List watches"
        pauseParser   = command "pause"                            $
                        info (Pause . wn <$> argument str wnLabel) $
                        progDesc "Pause watch"
        unPauseParser = command "unpause"                            $
                        info (UnPause . wn <$> argument str wnLabel) $
                        progDesc "Unpause watch"
        checkInParser = command "checkin"                            $
                        info (CheckIn . wn <$> argument str wnLabel) $
                        progDesc "Check in watch"
        infoParser    = command "info"                               $
                        info (Info . wn <$> argument str wnLabel) $
                        progDesc "Get info about a watch"
        testParser    = command "test"                               $
                        info (Test . wn <$> argument str wnLabel) $
                        progDesc "Test the notifications for a watch"
        wn            = WatchName . pack
        wnLabel       = metavar "WATCH_NAME"
