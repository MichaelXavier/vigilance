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
import Data.Configurator (load, Worth(Required))
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import Network.Http.Client ( Hostname
                           , Port )
import Utils.Vigilance.Types

data Command = List              |
               Pause WatchName   |
               UnPause WatchName |
               CheckIn WatchName |
               Info WatchName deriving (Show, Eq)

data Options = Options { optCommand :: Command
                       , configPath :: FilePath } deriving (Show, Eq)

-- probably want to just trash these
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

type ClientCtxT m a = ReaderT ClientConfig m a

defaultHost :: Hostname
defaultHost = "localhost"

defaultPort' :: Port
defaultPort' = fromInteger . toInteger $ defaultPort

readClientConfig :: FilePath -> IO ClientConfig
readClientConfig = convertClientConfig <=< loadConfig

loadConfig :: FilePath -> IO CT.Config
loadConfig pth = load [Required pth]

--TODO: elimiate toplevel in config
convertClientConfig :: CT.Config -> IO ClientConfig
convertClientConfig cfg = ClientConfig <$> lookupHost <*> lookupPort
  where lookupHost = C.lookupDefault defaultHost cfg "vigilance.host"
        lookupPort = C.lookupDefault defaultPort' cfg "vigilance.port"