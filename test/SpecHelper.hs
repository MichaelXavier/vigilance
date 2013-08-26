{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SpecHelper ( module Utils.Vigilance.Types
                  , (<$>)
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
                  , module Network.Mail.Mime
                  , module Control.Lens
                  , module Data.Monoid
                  , module Test.Hspec
                  , module Test.Hspec.Expectations
                  , module Test.Hspec.QuickCheck
                  , module Test.QuickCheck.Property.Common
                  , module Test.QuickCheck.Property.Monoid) where

import Control.Applicative ( (<$>)
                           , (<*>)
                           , pure)
import Control.Lens hiding (elements)
import Data.DeriveTH
import Data.Derive.Arbitrary (makeArbitrary)
import Data.List (nubBy)
import Data.Monoid
import qualified Data.Set as S
import Data.Text ( Text
                 , pack)
import Data.Time.Clock.POSIX (POSIXTime)
import Network.Mail.Mime
import Test.Hspec
import Test.Hspec.Expectations
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property.Common
import Test.QuickCheck.Property.Monoid
import Text.InterpolatedString.Perl6 (qc)
import Utils.Vigilance.Types


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
