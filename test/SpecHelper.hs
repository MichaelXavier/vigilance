{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SpecHelper ( module Utils.Vigilance.Types
                  , (<$>)
                  , (<*>)
                  , pure
                  , POSIXTime
                  , baseWatch
                  , baseNewWatch
                  , bumpTime
                  , NonEmptyList(..)
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
import Control.Lens
import Data.Monoid
import Data.Text ( Text
                 , pack)
import Data.Time.Clock.POSIX (POSIXTime)
import Utils.Vigilance.Types
import Test.Hspec
import Test.Hspec.Expectations
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property.Common
import Test.QuickCheck.Property.Monoid

import Data.DeriveTH
import Data.Derive.Arbitrary (makeArbitrary)

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

instance Arbitrary NewWatch where
  arbitrary = Watch <$> pure ()
                    <*> genText
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary

instance Arbitrary EWatch where
  arbitrary = Watch <$> arbitrary
                    <*> genText
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary

instance Arbitrary EmailAddress where
  arbitrary = EmailAddress <$> genText

genText :: Gen Text
genText = pack <$> genString

genString :: Gen String
genString = (listOf $ choose charRange)
  where charRange = ('\32', '\128')
