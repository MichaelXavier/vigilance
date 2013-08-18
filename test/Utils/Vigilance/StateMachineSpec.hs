module Utils.Vigilance.StateMachineSpec (spec) where

import Data.Map (fromList)

import Control.Monad.Identity
import Utils.Vigilance.StateMachine

import SpecHelper

spec :: Spec
spec = parallel $ do
  describe "all valid transitions" $ do
    it "returns the correct final state" $
      snd <$> runValidMachine `shouldBe` Just Final
    it "fires the expected callbacks" $ do
      fst <$> runValidMachine `shouldBe` Just [ Nothing
                                              , Just "trans to final"]
  describe "invalid transitions" $ do
    it "errors out" $
      runInvalidMachine `shouldBe` Nothing

runValidMachine = runStateMachine Init stateMachine doStuff
  where doStuff = do x <- transition Middle
                     y <- transition Final
                     return [x, y]

runInvalidMachine = runStateMachine Init stateMachine doStuff
  where doStuff = do y <- transition Final
                     return [y]

stateMachine :: StateMachine TestState Maybe String --dunno
stateMachine = StateMachine $ fromList [ ((Init, Middle), Nothing)
                                       , ((Middle, Final), Just $ return "trans to final") ]


data TestState = Init | Middle | Final deriving (Show, Eq, Ord)
