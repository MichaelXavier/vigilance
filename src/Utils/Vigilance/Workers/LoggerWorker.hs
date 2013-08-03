module Utils.Vigilance.Workers.LoggerWorker ( runWorker ) where

import Control.Concurrent.Chan ( readChan )
import Control.Monad ( forever
                     , (<=<) )
import System.IO (openFile
                 , withFile
                 , IOMode(AppendMode))
import System.Log.FastLogger ( Logger
                             , LogStr
                             , mkLogger
                             , loggerPutStr )

import Utils.Vigilance.Types

runWorker :: FilePath -> LogChan -> IO ()
runWorker path q = do logger <- openLogger path
                      let logMessages' = logMessages logger q
                      forever logMessages'

logMessages :: Logger -> LogChan -> IO ()
logMessages logger = loggerPutStr logger <=< readChan

openLogger :: FilePath -> IO Logger
openLogger path = mkLogger flushEveryLine =<< openFile path AppendMode
  where flushEveryLine = True
