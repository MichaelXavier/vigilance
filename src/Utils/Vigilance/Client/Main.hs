{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Prelude (FilePath)
import ClassyPrelude hiding (FilePath)
import Control.Monad ((<=<))
import Control.Monad.Trans.Reader (runReaderT)
import Options.Applicative
import Options.Applicative.Builder.Internal ( CommandFields
                                            , Mod )

import Utils.Vigilance.Client.Client
import Utils.Vigilance.Client.Config
import Utils.Vigilance.Types

main :: IO ()
main = runOptions =<< execParser opts

runOptions :: Options -> IO ()
runOptions Options {..} = runReaderT (runCommand optCommand) =<< readClientConfig configPath

runCommand :: Command -> ClientCtxT IO ()
runCommand List = either displayError (lift . displayList) =<< getList
runCommand _ = undefined

--TODO: better error reporting
displayError :: VError -> ClientCtxT IO ()
displayError = lift . print

opts :: ParserInfo Options
opts = info (helper <*> optionsParser) banner
  where banner = fullDesc <>
                 progDesc "TODO: full desc" <>
                 header "vigilance - tool for managing vigilance watches locally or remotely."

optionsParser :: Parser Options
optionsParser = Options <$> subparser commandParser
               <*> pathParser

pathParser :: Parser FilePath
pathParser = parser <|> pure defaultConfigPath
  where parser = strOption $ long "config"  <>
                             short 'c'      <>
                             metavar "FILE" <>
                             help "Config file. Defaults to ~/.vigilance"


commandParser :: Mod CommandFields Command
commandParser = listParser    <>
                pauseParser   <>
                unPauseParser <>
                checkInParser <>
                infoParser
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
                        progDesc "Check in watch"
        wn            = WatchName . pack
        wnLabel       = metavar "WATCH_NAME"


