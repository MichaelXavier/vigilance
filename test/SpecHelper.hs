{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SpecHelper ( module Utils.Vigilance.Types
                  , module Data.Monoid
                  , module Test.Hspec
                  , module Test.Hspec.Expectations
                  , module Test.Hspec.QuickCheck
                  , module Test.QuickCheck.Property.Monoid) where

import Data.Monoid
import Utils.Vigilance.Types
import Test.Hspec
import Test.Hspec.Expectations
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property.Monoid

import Data.DeriveTH
import Data.Derive.Arbitrary (makeArbitrary)

$(derive makeArbitrary ''WatchState)
