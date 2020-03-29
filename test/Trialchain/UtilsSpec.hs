{-# OPTIONS_GHC -fno-warn-orphans #-}
module Trialchain.UtilsSpec where

import Crypto.Random
import Data.Text (pack)
import Test.Aeson.GenericSpecs
import Test.Hspec
import Test.QuickCheck

import Trialchain.Identity
import Trialchain.Utils

instance Arbitrary Identity where
  arbitrary = Identity <$> arbitrary <*> arbitrary

genKeyPair :: Gen (PublicKey, PrivateKey)
genKeyPair = do
  seedInt <- arbitrary
  pure $ fst $ withDRG (drgNewSeed (seedFromInteger seedInt)) generateKeyPair

instance Arbitrary Hash where
  arbitrary = hashOf . pack <$> arbitrary

instance Arbitrary PublicKey where
  arbitrary = fst <$> genKeyPair

instance Arbitrary PrivateKey where
  arbitrary = snd <$> genKeyPair

spec :: Spec
spec = describe "Crypto utilities" $ do
  roundtripAndGoldenSpecs (Proxy :: Proxy Hash)
  roundtripAndGoldenSpecs (Proxy :: Proxy PublicKey)
  roundtripAndGoldenSpecs (Proxy :: Proxy PrivateKey)
  roundtripAndGoldenSpecs (Proxy :: Proxy Identity)
