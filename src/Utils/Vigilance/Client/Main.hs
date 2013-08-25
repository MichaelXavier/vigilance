{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Prelude (FilePath)
import ClassyPrelude hiding (FilePath)
import Control.Monad ((<=<))
import Data.Configurator (load, Worth(Required))
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import Network.Http.Client ( Hostname
                           , Port )
import Options.Applicative
import Options.Applicative.Builder.Internal ( CommandFields
                                            , Mod )

import Utils.Vigilance.Types

main :: IO ()
main = runOptions =<< execParser opts

runOptions :: Options -> IO ()
runOptions Options {..} = runCommand optCommand =<< readConfig configPath

readConfig :: FilePath -> IO ClientConfig
readConfig = convertClientConfig <=< loadConfig

loadConfig :: FilePath -> IO CT.Config
loadConfig pth = load [Required pth]

--TODO: elimiate toplevel in config
convertClientConfig :: CT.Config -> IO ClientConfig
convertClientConfig cfg = ClientConfig <$> lookupHost <*> lookupPort
  where lookupHost = C.lookupDefault defaultHost cfg "vigilance.host"
        lookupPort = C.lookupDefault defaultPort' cfg "vigilance.port"
        defaultPort' = fromInteger . toInteger $ defaultPort

defaultHost :: Hostname
defaultHost = "localhost"

runCommand :: Command -> ClientConfig -> IO ()
runCommand = undefined

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


data Command = List              |
               Pause WatchName   |
               UnPause WatchName |
               CheckIn WatchName |
               Info WatchName deriving (Show, Eq)

data Options = Options { optCommand :: Command
                       , configPath :: FilePath } deriving (Show, Eq)

instance Monoid Command where
  mempty = List
  List `mappend` b = b
  a `mappend` List = a

instance Monoid Options where
  mempty = Options mempty defaultConfigPath
  (Options ca pa) `mappend` (Options cb pb) = Options cmd pth
    where cmd = ca <> cb
          pth
            | pb == defaultConfigPath = pa
            | otherwise               = pb

defaultConfigPath :: FilePath
defaultConfigPath = "$(HOME)/.vigilance"

data ClientConfig = ClientConfig { serverHost :: Hostname
                                 , serverPort :: Port } deriving (Show, Eq)
