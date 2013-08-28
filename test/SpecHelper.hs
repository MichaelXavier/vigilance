{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SpecHelper ( (<$>)
                  , (<*>)
                  , pure
                  , qc
                  , POSIXTime
                  , baseWatch
                  , baseNewWatch
                  , bumpTime
                  , NonEmptyList(..)
                  , Text
                  , UniqueWatches(..)
                  , module X) where

import Control.Applicative ( (<$>)
                           , (<*>)
                           , pure)
import Data.DeriveTH
import Data.Derive.Arbitrary (makeArbitrary)
import Data.List (nubBy)
import qualified Data.Set as S
import Data.Text ( Text
                 , pack)
import Data.Time.Clock.POSIX (POSIXTime)

import Control.Lens as X hiding (elements)
import Data.Monoid as X
import Network.Mail.Mime as X
import Test.Hspec as X
import Test.Hspec.Expectations as X
import Test.Hspec.QuickCheck as X
import Text.InterpolatedString.Perl6 (qc)
import Test.QuickCheck
import Test.QuickCheck.Property.Common as X
import Test.QuickCheck.Property.Monoid as X
import Utils.Vigilance.Types as X



baseWatch :: EWatch
baseWatch = Watch (ID 1) "whatever" (Every 1 Seconds) mempty []

baseNewWatch :: NewWatch
baseNewWatch = Watch () "whatever" (Every 1 Seconds) mempty []

bumpTime :: Integer -> WatchState -> WatchState
bumpTime n (Active t) = Active (t + fromInteger n)
bumpTime _ s          = s

instance Arbitrary POSIXTime where
  arbitrary = fromInteger <$> arbitrary

$(derive makeArbitrary ''WatchState)
$(derive makeArbitrary ''WatchInterval)
$(derive makeArbitrary ''NotificationPreference)
$(derive makeArbitrary ''TimeUnit)
$(derive makeArbitrary ''ID)
$(derive makeArbitrary ''LogCfg)
$(derive makeArbitrary ''Config)

instance Arbitrary NewWatch where
  arbitrary = Watch <$> pure ()
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary

instance Arbitrary EWatch where
  arbitrary = Watch <$> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary

instance Arbitrary WatchName where
  arbitrary = WatchName <$> genText

instance Arbitrary EmailAddress where
  arbitrary = EmailAddress <$> genText

genText :: Gen Text
genText = pack <$> genString

genString :: Gen String
genString = (listOf $ choose charRange)
  where charRange = ('\32', '\128')

newtype UniqueWatches = UniqueWatches [NewWatch]
 deriving ( Eq, Ord, Show)

instance Arbitrary UniqueWatches where
  arbitrary = UniqueWatches . nubBy same <$> arbitrary
    where a `same` b = a ^. watchId == b ^. watchId && a ^. watchName == b ^. watchName
