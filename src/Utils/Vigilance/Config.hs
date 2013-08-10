{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils.Vigilance.Config ( configNotifiers
                              , convertConfig
                              , loadConfig) where

import ClassyPrelude hiding (FilePath)
import Control.Applicative ( (<$>)
                           , (<*>) )
import Control.Monad -- ((<=<))
import Control.Monad.Reader (ask)
import Control.Lens
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import Data.Maybe (catMaybes)
import GHC.IO (FilePath)
import qualified Utils.Vigilance.Notifiers.Email as E
import qualified Utils.Vigilance.Notifiers.Log   as L
import Utils.Vigilance.Types

configNotifiers :: Config -> LogCtx IO [Notifier]
configNotifiers cfg = do logNotifier        <- L.notify <$> ask
                         let mEmailNotifier = E.notify . E.EmailContext <$> cfg ^. configFromEmail
                         return $ catMaybes [Just logNotifier, mEmailNotifier]

loadConfig :: FilePath -> IO Config
loadConfig = convertConfig <=< C.load . return . CT.Required

-- basically no point to this mappend at present
convertConfig :: CT.Config -> IO Config
convertConfig cfg = mempty <> Config <$> lud defaultAcidPath "vigilance.acid_path"
                                     <*> (toEmailAddress <$> lu "vigilance.from_email")
                                     <*> (lud defaultPort "vigilance.port")
                                     <*> lud defaultLogPath "vigilance.log_path"
  where lu             = C.lookup cfg
        lud d          = C.lookupDefault d cfg
        toEmailAddress = fmap (EmailAddress . pack)
