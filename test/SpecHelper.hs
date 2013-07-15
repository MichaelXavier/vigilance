{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SpecHelper ( module Utils.Vigilance.Types
                  , (<$>)
                  , (<*>)
                  , pure
                  , module Control.Lens
                  , module Data.Monoid
                  , module Test.Hspec
                  , module Test.Hspec.Expectations
                  , module Test.Hspec.QuickCheck
                  , module Test.QuickCheck.Property.Monoid) where

import Control.Applicative ( (<$>)
                           , (<*>)
                           , pure)
import Control.Lens
import Data.Monoid
import Data.Text ( Text
                 , pack)
import Utils.Vigilance.Types
import Test.Hspec
import Test.Hspec.Expectations
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property.Monoid

import Data.DeriveTH
import Data.Derive.Arbitrary (makeArbitrary)

$(derive makeArbitrary ''WatchState)

$(derive makeArbitrary ''WatchInterval)
$(derive makeArbitrary ''NotificationPreference)
$(derive makeArbitrary ''TimeUnit)

instance Arbitrary WatchReport where
  arbitrary = WatchReport <$> arbitrary
                          <*> pure Nothing --FIXME

instance Arbitrary NewWatch where
  arbitrary = Watch <$> pure ()
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
