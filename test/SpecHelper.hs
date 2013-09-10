{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SpecHelper ( (<$>)
                  , (<*>)
                  , atomically
                  , pure
                  , qc
                  , POSIXTime
                  , baseWatch
                  , baseNewWatch
                  , baseFN
                  , bumpTime
                  , NonEmptyList(..)
                  , Text
                  , UniqueWatches(..)
                  , shouldParseJSON
                  , shouldGenerateJSON
                  , module X) where

import ClassyPrelude
import Control.Applicative ( (<$>)
                           , (<*>)
                           , pure)
import Data.Aeson ( FromJSON
                  , eitherDecode'
                  , ToJSON(toJSON)
                  , Value )
import qualified Data.ByteString.Lazy as LBS
import Data.DeriveTH
import Data.Derive.Arbitrary (makeArbitrary)
import qualified Data.Set as S
import Data.Time.Clock.POSIX (POSIXTime)

import Control.Concurrent.STM (atomically)
import Control.Monad.Loops as X
import Control.Lens as X hiding (elements)
import Data.Monoid as X
import Network.Http.Client (URL)
import Network.Mail.Mime as X
import Test.Hspec as X
import Test.Hspec.Expectations as X
import Test.Hspec.QuickCheck as X
import Text.InterpolatedString.Perl6 (qc)
import Test.QuickCheck
import Test.QuickCheck.Property.Common as X
import Test.QuickCheck.Property.Monoid as X
import Utils.Vigilance.Types as X
import Utils.Vigilance.Utils as X



baseWatch :: EWatch
baseWatch = Watch (ID 1) "whatever" (Every 1 Seconds) mempty []

baseNewWatch :: NewWatch
baseNewWatch = Watch () "whatever" (Every 1 Seconds) mempty []

baseFN :: FailedNotification
baseFN = FailedNotification baseWatch (HTTPNotification "example.com") (FailedByCode 500) 0

bumpTime :: Integer -> WatchState -> WatchState
bumpTime n (Active t) = Active (t + fromInteger n)
bumpTime _ s          = s

instance Arbitrary POSIXTime where
  arbitrary = fromInteger <$> arbitrary

genString :: Gen String
genString = listOf $ choose charRange
  where charRange = ('\32', '\128')

genBS :: Gen ByteString
genBS = encodeUtf8 . pack <$> genString

instance Arbitrary URL where
  arbitrary = genBS

$(derive makeArbitrary ''TimeUnit)

instance Arbitrary WatchInterval where
  arbitrary = do Positive n <- arbitrary
                 Every <$> pure n <*> arbitrary

genText :: Gen Text
genText = pack <$> genString

-- Dubious typesafety here
instance Arbitrary NotificationError where
  arbitrary = oneof [FailedByException <$> genText, FailedByCode <$> arbitrary]

$(derive makeArbitrary ''WatchState)
$(derive makeArbitrary ''NotificationPreference)
$(derive makeArbitrary ''ID)
$(derive makeArbitrary ''LogCfg)
$(derive makeArbitrary ''Config)
$(derive makeArbitrary ''FailedNotification)

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

newtype UniqueWatches = UniqueWatches [NewWatch]
 deriving ( Eq, Ord, Show)

instance Arbitrary UniqueWatches where
  arbitrary = UniqueWatches . nubBy same <$> arbitrary
    where a `same` b = a ^. watchId == b ^. watchId && a ^. watchName == b ^. watchName

shouldParseJSON :: (FromJSON a, Show a, Eq a) => LBS.ByteString -> a -> Expectation
shouldParseJSON str obj = eitherDecode' str `shouldBe` Right obj

shouldGenerateJSON :: (ToJSON a) => a -> Value -> Expectation
shouldGenerateJSON x value = toJSON x `shouldBe` value
