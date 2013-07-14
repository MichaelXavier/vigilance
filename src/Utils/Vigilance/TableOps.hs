module Utils.Vigilance.TableOps (createWatch
                                , deleteWatch
                                , findWatch
                                , emptyTable) where

import Control.Lens
import Data.Table ( insert'
                  , with
                  , empty
                  , deleteWith)
import Utils.Vigilance.Types

createWatch :: NewWatch -> WatchTable -> (EWatch, WatchTable)
createWatch w = insert' $ w & watchId .~ (ID 0)

deleteWatch :: ID -> WatchTable -> WatchTable
deleteWatch i table = table & deleteWith WatchID (==) i

findWatch :: ID -> WatchTable -> Maybe EWatch
findWatch i table = table ^. at i

--watchLens = ix

emptyTable :: WatchTable
emptyTable = empty
