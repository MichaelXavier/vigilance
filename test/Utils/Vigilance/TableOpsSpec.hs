module Utils.Vigilance.TableOpsSpec (spec) where

import Utils.Vigilance.TableOps
import SpecHelper

spec :: Spec
spec = do
  describe "table properties" $ do
    prop "it deletes data that is known" $ \w ->
      let (w', table) = createWatch w emptyTable
      in deleteWatch (w' ^. watchId) table == emptyTable
    prop "data is findable after insert" $ \w ->
      let (w', table) = createWatch w emptyTable
      in findWatch (w' ^. watchId) table == Just w'
    prop "watchLens finds records" $ \w newInterval ->
      let (w', table) = createWatch w emptyTable
          wId         = w' ^. watchId
          table'      = watchLens wId table (watchInterval .~ newInterval)
          in (view watchInterval <$> findWatch wId table') == Just newInterval
