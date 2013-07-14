module Utils.Vigilance.Worker ( workForeverWith
                              , workForever) where

import Control.Monad (forever, (<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Error
import Control.Exception.Base (SomeException)
import Control.Concurrent.Async ( waitCatch
                                , async)

workForeverWith :: (SomeException -> IO ()) -> IO () -> IO ()
workForeverWith handler action = forever $ waitHandle =<< async action
  where waitHandle = either handler return <=< waitCatch

-- eats errors
workForever :: IO () -> IO ()
workForever = workForeverWith (const $ return ())
