module Utils.Vigilance.TableOpsSpec (spec) where

import Utils.Vigilance.TableOps
import SpecHelper

spec :: Spec
spec = do
  describe "create/delete cycle" $ do
    prop "it reliably inserts and deletes" $ \w ->
      let (w', table) = createWatch w emptyTable
      in deleteWatch (w' ^. watchId) table == emptyTable
