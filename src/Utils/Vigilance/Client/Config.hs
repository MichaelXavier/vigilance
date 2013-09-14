{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Utils.Vigilance.Client.Config ( Command(..)
                                     , Options(..)
                                     , ClientConfig(..)
                                     , ClientCtxT
                                     , defaultHost
                                     , readClientConfig
                                     , defaultPort'
                                     , defaultConfigPath ) where

import Prelude (FilePath)
import ClassyPrelude hiding (FilePath)
import Control.Monad ((<=<))
import Control.Monad.Trans.Reader
import Data.Configurator (load, Worth(Optional))
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import Network.Http.Client ( Hostname
                           , Port )
import Utils.Vigilance.Types

data Command = List              |
               Pause WatchName   |
               UnPause WatchName |
               CheckIn WatchName |
               Info WatchName    |
               Test WatchName deriving (Show, Eq)

data Options = Options { optCommand :: Command
                       , configPath :: FilePath } deriving (Show, Eq)

defaultConfigPath :: FilePath
defaultConfigPath = vigilanceDir <> "/client.conf"

data ClientConfig = ClientConfig { serverHost :: Hostname
                                 , serverPort :: Port } deriving (Show, Eq)

type ClientCtxT m a = ReaderT ClientConfig m a

defaultHost :: Hostname
defaultHost = "localhost"

defaultPort' :: Port
defaultPort' = fromInteger . toInteger $ defaultPort

readClientConfig :: FilePath -> IO ClientConfig
readClientConfig = convertClientConfig <=< loadConfig

loadConfig :: FilePath -> IO CT.Config
loadConfig pth = load [Optional pth]

--TODO: elimiate toplevel in config
convertClientConfig :: CT.Config -> IO ClientConfig
convertClientConfig cfg = ClientConfig <$> lookupHost <*> lookupPort
  where lookupHost = C.lookupDefault defaultHost cfg "vigilance.host"
        lookupPort = C.lookupDefault defaultPort' cfg "vigilance.port"
